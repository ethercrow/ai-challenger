{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger.WebApp
    ( webApp
    ) where

import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import qualified Data.Aeson as A
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Vector.Extended as V
import qualified Network.Wai as Wai
import Network.Wai.Metrics
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Path
import Servant
import Servant.Docs
import Servant.HTML.Lucid
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Markdown as MD
import Text.Read

import AIChallenger.HTML
import AIChallenger.Match
import AIChallenger.StateVar
import AIChallenger.Types

turnLimit :: Turn
turnLimit = Turn 200

type WebAPI = JSONAPI :<|> HTMLAPI

type JSONAPI
    = "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot
    :<|> "launch-round-robin-tournament" :> Capture "mapName" MapName :> Post '[JSON] Tournament
    :<|> "launch-training" :> Capture "mapName" MapName :> Capture "botName" T.Text :> Post '[JSON] Tournament
    :<|> "match" :> Capture "matchId" MatchId :> Get '[JSON] Match

type HTMLAPI
    = Get '[HTML] MainPage
    :<|> "matches" :> Capture "matchId" MatchId :> Get '[HTML] MatchPage
    :<|> "tournaments" :> Capture "tournamentId" TournamentId :> Get '[HTML] TournamentPage
    :<|> "help" :> Get '[HTML, PlainText] APIDocs

webApp :: Game game => game -> StateVar -> (Maybe WaiMetrics) -> Path Abs Dir -> Wai.Application
webApp game stateVar waiMetrics dashboardDir =
    websocketsOr defaultConnectionOptions
        (wsApp stateVar)
        (httpApp game stateVar waiMetrics dashboardDir)

wsSendJSON :: A.ToJSON a => Connection -> a -> IO ()
wsSendJSON conn x = sendDataMessage conn (Text (A.encode x))

wsApp :: StateVar -> ServerApp
wsApp stateVar pendingConnection = do
    (state, chan) <- subscribeToStateUpdates stateVar
    let reqPath = requestPath (pendingRequest pendingConnection)
    case reqPath of
        "state" -> do
            conn <- acceptRequest pendingConnection
            BS.putStrLn ("Incoming websocket request " <> reqPath)
            wsSendJSON conn state
            forever $ do
                update <- readChan chan
                wsSendJSON conn update
        "/tournaments" -> do
            conn <- acceptRequest pendingConnection
            mapM_ (wsSendJSON conn) (ssTournaments state)
            forever $ do
                update <- readChan chan
                case update of
                    AddTournament t -> wsSendJSON conn t
                    _ -> return ()
        (BS.split '/' -> ["", "tournament", (fmap TournamentId . readMaybe . BS.unpack) -> Just tid]) -> do
            -- TODO: better lookup performance
            case V.filter ((== tid) . tId) (ssTournaments state) of
                [] -> do
                    BS.putStrLn ("websocket request " <> reqPath <> " rejected, because 404")
                    rejectRequest pendingConnection (BS.pack ("no tournament with " <> show tid))
                [t] -> do
                    conn <- acceptRequest pendingConnection
                    let mids = S.fromList (V.toList (tMatchIds t))
                        completedMatches = V.filter ((`S.member` mids) . matchId) (ssMatches state)
                        remainingMids = mids `S.difference` (S.fromList (V.toList (V.map matchId completedMatches)))
                    wsSendJSON conn t
                    mapM_ (wsSendJSON conn) completedMatches
                    let go (S.null -> True) = return ()
                        go stillRemaining = do
                            update <- readChan chan
                            case update of
                                AddMatch m | matchId m `S.member` stillRemaining -> do
                                    wsSendJSON conn m
                                    go (S.delete (matchId m) stillRemaining)
                                _ -> return ()
                    go remainingMids
                _ -> error "tournament id collision, this should never happen"
        _ -> do
            BS.putStrLn ("Rejected unknown websocket request " <> reqPath)
            rejectRequest pendingConnection ("Unknown path " <> reqPath)

httpApp :: Game game => game -> StateVar -> Maybe WaiMetrics -> Path Abs Dir -> Wai.Application
httpApp game stateVar waiMetrics dashboardDir = do
    let jsonHandlers = readStateVar stateVar
            :<|> postBot stateVar
            :<|> launchRoundRobinTournament game stateVar
            :<|> launchTraining game stateVar
            :<|> match stateVar
        htmlHandlers = mainPage stateVar
            :<|> matchPage stateVar
            :<|> tournamentPage stateVar
            :<|> helpPage
        handlers = jsonHandlers :<|> htmlHandlers
        middleware :: [Wai.Application -> Wai.Application]
        middleware = catMaybes [metrics <$> waiMetrics, Just logStdout, Just simpleCors]
    foldr ($) (serve (Proxy :: Proxy WebAPI) handlers) middleware

tournamentPage :: MonadIO m => StateVar -> TournamentId -> m TournamentPage
tournamentPage stateVar tid = do
    state <- readStateVar stateVar
    -- TODO: better lookup performance
    case V.filter ((== tid) . tId) (ssTournaments state) of
        [] -> error ("no tournament with " <> show tid)
        [t] -> do
            let matches = V.filter ((`elem` tMatchIds t) . matchId) (ssMatches state)
                bots = V.nub (V.concatMap matchBots matches)
            return (TournamentPage t matches)
        _ -> error "tournament id collision, this should never happen"

postBot :: StateVar -> Bot -> EitherT ServantErr IO Bot
postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    newBotOrError <- addBot bot stateVar
    case newBotOrError of
        Right newBot -> return newBot
        Left err -> left err400 { errReasonPhrase = err }

checkMapName :: Game game => game -> MapName -> EitherT ServantErr IO ()
checkMapName game mapName@(MapName m) = do
    availableMaps <- liftIO (gameAvailableMaps game)
    liftIO $ print availableMaps
    when (mapName `notElem` availableMaps)
        (left err404 {
            errReasonPhrase = "Map " <> T.unpack m <> " is not available"
            })

launchRoundRobinTournament :: Game game => game -> StateVar -> MapName -> EitherT ServantErr IO Tournament
launchRoundRobinTournament game stateVar mapName = do
    checkMapName game mapName
    launchTournament stateVar RoundRobin mapName (launchBotsAndSimulateMatch game mapName turnLimit)

launchTraining :: Game game => game -> StateVar -> MapName -> BotName -> EitherT ServantErr IO Tournament
launchTraining game stateVar mapName name = do
    checkMapName game mapName
    launchTournament stateVar (Training name) mapName (launchBotsAndSimulateMatch game mapName turnLimit)

launchTournament :: StateVar -> TournamentKind -> MapName ->
     (V.Vector Bot -> TournamentId -> MatchId -> IO Match)
     -> EitherT ServantErr IO Tournament
launchTournament stateVar tournamentKind _mapName play = do
    -- TODO: include mapName in tournament record,
    --       spectators would probably like to know what map is used in a tournament
    bots <- ssBots <$> readStateVar stateVar
    let pairs = case tournamentKind of 
            RoundRobin -> [V.fromList [b1, b2] | b1 <- bots, b2 <- bots, b1 < b2]
            Training name ->
                let myBot = V.head (V.filter ((== name) . botName) bots)
                in [V.fromList [myBot, other] | other <- bots, myBot /= other]
    let matchCount = length pairs
    if matchCount < 1
    then left err400 { errReasonPhrase = "Not enough bots for a tournament" }
    else do
        tournament@(Tournament tid _ _ mids) <- mkTournament stateVar tournamentKind matchCount bots
        _ <- liftIO . forkIO $ do
            putStrLn (show tid <> " started")
            forM_ (V.zip mids pairs) $ \(mId, pair) -> do
                result <- play pair tid mId
                putStrLn ("Finished " <> show mId)
                void $ addMatch stateVar result
            putStrLn (show tid <> " finished")
        return tournament

match :: MonadIO m => StateVar -> MatchId -> m Match
match stateVar mid = do
    matches <- ssMatches <$> readStateVar stateVar
    -- TODO: better than O(n) lookup
    case V.filter ((== mid) . matchId) matches of
        [] -> error ("no match with " <> show mid)
        [m] -> return m
        _ -> error "match id collision, this should never happen"

matchPage :: MonadIO m => StateVar -> MatchId -> m MatchPage
matchPage stateVar mid = do
    m <- match stateVar mid
    replayText <- liftIO (TIO.readFile (toFilePath (matchReplayPath m)))
    return (MatchPage m replayText)

replay :: MonadIO m => StateVar -> MatchId -> m ReplayText
replay stateVar mid = do
    matches <- ssMatches <$> readStateVar stateVar
    -- TODO: better than O(n) lookup
    case V.filter ((== mid) . matchId) matches of
        [] -> error ("no match with " <> show mid)
        [m] -> do
            ReplayText <$> liftIO (TLIO.readFile (toFilePath (matchReplayPath m)))
        _ -> error "match id collision, this should never happen"

mainPage :: MonadIO m => StateVar -> m MainPage
mainPage stateVar = MainPage <$> readStateVar stateVar

helpPage :: EitherT ServantErr IO APIDocs
helpPage = return (APIDocs (TL.pack (markdown (docs (Proxy :: Proxy JSONAPI)))))

newtype APIDocs = APIDocs TL.Text

newtype LaunchTournamentReply = LaunchTournamentReply String

newtype ReplayText = ReplayText TL.Text

instance MimeRender PlainText APIDocs where
    mimeRender _ (APIDocs s) = TLE.encodeUtf8 s

instance MimeRender PlainText ReplayText where
    mimeRender _ (ReplayText s) = TLE.encodeUtf8 s

instance ToSample ReplayText ReplayText where
    toSample _ = Just (ReplayText "4 8 P S\n4 9 P S\n4 9 S S")

instance MimeRender HTML APIDocs where
    mimeRender _ (APIDocs s) = renderHtml (MD.markdown MD.def s)

instance ToSample APIDocs APIDocs where
    toSample _ = Just (APIDocs "This very page")

instance MimeRender PlainText LaunchTournamentReply where
    mimeRender _ (LaunchTournamentReply s) = BSL.pack s

instance ToSample LaunchTournamentReply LaunchTournamentReply where
    toSample _ = Just (LaunchTournamentReply "42 matches scheduled")

instance ToSample ServerState ServerState where
    toSample _ = do
        rocky <- Bot "Rocky" . ExecutableBot <$> parseAbsFile "/tmp/rocky.py"
        pepper <- Bot "Pepper" . ExecutableBot <$> parseAbsFile "/tmp/pepper.py"
        scarlett <- Bot "Scarlett" . ExecutableBot <$> parseAbsFile "/tmp/scarlett.py"
        let bots = [rocky, pepper, scarlett]
            matches = mempty
            tournaments = mempty
        return (ServerState (MatchId (V.length matches)) (TournamentId 0) bots matches tournaments)

instance ToSample Match Match where
    toSample _ = do
        bots <-
            (\b1 b2 -> pure b1 <> pure b2)
            <$> (toSample (Proxy :: Proxy Bot))
            <*> (toSample (Proxy :: Proxy Bot))
        replayPath <- parseAbsFile "/tmp/0.replay"
        let winners = V.take 1 bots
        return (Match
            (MatchId 0)
            (TournamentId 0)
            bots
            winners
            Elimination
            replayPath)

instance ToSample Tournament Tournament where
    toSample _ = do
        rocky <- Bot "Rocky" . ExecutableBot <$> parseAbsFile "/tmp/rocky.py"
        pepper <- Bot "Pepper" . ExecutableBot <$> parseAbsFile "/tmp/pepper.py"
        return (Tournament (TournamentId 0) RoundRobin (V.fromList [rocky, pepper]) (pure (MatchId 0)))


instance ToSample Bot Bot where
    toSample _ = Bot "Randy" . ExecutableBot <$> parseAbsFile "/home/randy/src/shiny_metal_bot.sh"

instance ToSample MainPage MainPage where
    toSample _ = MainPage <$> toSample (Proxy :: Proxy ServerState)

instance ToCapture (Capture "matchId" MatchId) where
    toCapture _ = DocCapture "matchId" "Integer id of the match"

instance ToCapture (Capture "tournamentId" TournamentId) where
    toCapture _ = DocCapture "tournamentId" "Integer id of the tournament"

instance ToCapture (Capture "botName" T.Text) where
    toCapture _ = DocCapture "botName" "Name of the bot that will play against all other bots"

instance ToCapture (Capture "mapName" MapName) where
    toCapture _ = DocCapture "mapName" "Name of the map to be used in a tournament"

instance ToSample ServerStateUpdate ServerStateUpdate where
    toSample _ = Nothing
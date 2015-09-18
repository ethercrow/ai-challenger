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
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Vector as V
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

import AIChallenger.HTML
import AIChallenger.Match
import AIChallenger.StateVar
import AIChallenger.Types

turnLimit :: Turn
turnLimit = Turn 200

type WebAPI
    = Get '[HTML] MainPage
    :<|> "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot

    :<|> "launch-round-robin-tournament" :> Post '[JSON] Tournament
    :<|> "launch-training" :> Capture "botName" T.Text :> Post '[JSON] Tournament

    :<|> "match" :> Capture "matchId" MatchId :> Get '[JSON] Match
    :<|> "replay" :> Capture "matchId" MatchId :> Get '[PlainText] ReplayText

    :<|> "dashboard" :> Raw
    :<|> "css" :> Raw
    :<|> "images" :> Raw
    :<|> "js" :> Raw

    :<|> "help" :> Get '[HTML, PlainText] APIDocs

webApp :: Game game => game -> StateVar -> (Maybe WaiMetrics) -> Path Abs Dir -> Wai.Application
webApp game stateVar waiMetrics dashboardDir =
    websocketsOr defaultConnectionOptions
        (wsApp stateVar)
        (httpApp game stateVar waiMetrics dashboardDir)

wsApp :: StateVar -> ServerApp
wsApp stateVar pendingConnection = do
    conn <- acceptRequest pendingConnection
    (state, chan) <- subscribeToStateUpdates stateVar
    sendDataMessage conn (Binary (A.encode state))
    forever $ do
        update <- readChan chan
        sendDataMessage conn (Binary (A.encode update))

httpApp :: Game game => game -> StateVar -> (Maybe WaiMetrics) -> Path Abs Dir -> Wai.Application
httpApp game stateVar waiMetrics dashboardDir = do
    let handlers = mainPage stateVar
            :<|> readStateVar stateVar
            :<|> postBot stateVar
            :<|> launchRoundRobinTournament game stateVar
            :<|> launchTraining game stateVar
            :<|> match stateVar
            :<|> replay stateVar
            :<|> serveDirectory (toFilePath dashboardDir)
            :<|> serveDirectory (toFilePath (dashboardDir </> $(mkRelDir "css")))
            :<|> serveDirectory (toFilePath (dashboardDir </> $(mkRelDir "images")))
            :<|> serveDirectory (toFilePath (dashboardDir </> $(mkRelDir "js")))
            :<|> help
    let middleware :: [Wai.Application -> Wai.Application]
        middleware = catMaybes [metrics <$> waiMetrics, Just logStdout, Just simpleCors]
    foldr ($) (serve (Proxy :: Proxy WebAPI) handlers) middleware

postBot :: StateVar -> Bot -> EitherT ServantErr IO Bot
postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    newBotOrError <- addBot bot stateVar
    case newBotOrError of
        Right newBot -> return newBot
        Left err -> left err400 { errReasonPhrase = err }

launchRoundRobinTournament :: Game game => game -> StateVar -> EitherT ServantErr IO Tournament
launchRoundRobinTournament game stateVar = do
    launchTournament stateVar RoundRobin (launchBotsAndSimulateMatch game turnLimit)

launchTraining :: Game game => game -> StateVar -> BotName -> EitherT ServantErr IO Tournament
launchTraining game stateVar name = do
    launchTournament stateVar (Training name) (launchBotsAndSimulateMatch game turnLimit)

launchTournament :: StateVar -> TournamentKind ->
     (V.Vector Bot -> TournamentId -> MatchId -> IO Match)
     -> EitherT ServantErr IO Tournament
launchTournament stateVar tournamentKind play = do
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
        tournament@(Tournament tid _ mids) <- mkTournament stateVar tournamentKind matchCount
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

help :: EitherT ServantErr IO APIDocs
help = return (APIDocs (TL.pack (markdown (docs (Proxy :: Proxy WebAPI)))))

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
    toSample _ = Just (Tournament (TournamentId 0) RoundRobin (pure (MatchId 0)))

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

instance ToSample ServerStateUpdate ServerStateUpdate where
    toSample _ = Nothing
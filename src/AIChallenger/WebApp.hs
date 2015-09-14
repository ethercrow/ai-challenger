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
import Control.Concurrent.Chan.Unagi
import Control.Monad
import Control.Monad.Trans
import qualified Data.Aeson as A
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BSL
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
turnLimit = Turn 100

type WebAPI
    = Get '[HTML] MainPage
    :<|> "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot
    :<|> "launch-tournament" :> Post '[PlainText] LaunchTournamentReply
    :<|> "match" :> Capture "matchId" MatchId :> Get '[HTML] MatchPage
    :<|> "replay" :> Capture "matchId" MatchId :> Get '[PlainText] ReplayText
    :<|> "dashboard" :> Raw
    :<|> "css" :> Raw
    :<|> "images" :> Raw
    :<|> "js" :> Raw
    :<|> "help" :> Get '[HTML, PlainText] APIDocs

webApp :: Game game => game -> StateVar -> WaiMetrics -> Wai.Application
webApp game stateVar waiMetrics =
    websocketsOr defaultConnectionOptions
        (wsApp stateVar)
        (httpApp game stateVar waiMetrics)

wsApp :: StateVar -> ServerApp
wsApp stateVar pendingConnection = do
    conn <- acceptRequest pendingConnection
    (state, chan) <- subscribeToStateUpdates stateVar
    sendDataMessage conn (Binary (A.encode state))
    forever $ do
        update <- readChan chan
        sendDataMessage conn (Binary (A.encode update))

httpApp :: Game game => game -> StateVar -> WaiMetrics -> Wai.Application
httpApp game stateVar waiMetrics = do
    let handlers = mainPage stateVar
            :<|> readStateVar stateVar
            :<|> postBot stateVar
            :<|> launchTournament game stateVar
            :<|> match stateVar
            :<|> replay stateVar
            :<|> serveDirectory "dashboard"
            :<|> serveDirectory "dashboard/css"
            :<|> serveDirectory "dashboard/images"
            :<|> serveDirectory "dashboard/js"
            :<|> help
    let middleware :: [Wai.Application -> Wai.Application]
        middleware = [metrics waiMetrics, logStdout, simpleCors]
    foldr ($) (serve (Proxy :: Proxy WebAPI) handlers) middleware

postBot :: MonadIO m => StateVar -> Bot -> m Bot
postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    _ <- modifyStateVar stateVar (AddBot bot)
    return bot

launchTournament :: (Game game, MonadIO m) => game -> StateVar -> m LaunchTournamentReply
launchTournament game stateVar = do
    ServerState _ bots _ <- readStateVar stateVar
    let pairs = [V.fromList [b1, b2] | b1 <- bots, b2 <- bots, b1 < b2]
    _ <- liftIO . forkIO $ do
        V.forM_ pairs $ \pair -> do
            mid <- takeNextMatchId stateVar
            result <- launchBotsAndSimulateMatch game turnLimit pair mid
            putStrLn ("Finished " <> show mid)
            _ <- modifyStateVar stateVar (AddMatch result)
            return ()
    return (LaunchTournamentReply (show (length pairs) <> " matches scheduled"))

match :: MonadIO m => StateVar -> MatchId -> m MatchPage
match stateVar mid = do
    ServerState _ _ matches <- readStateVar stateVar
    -- TODO: better than O(n) lookup
    case V.filter ((== mid) . matchId) matches of
        [] -> error ("no match with " <> show mid)
        [m] -> do
            replayText <- liftIO (TLIO.readFile (toFilePath (matchReplayPath m)))
            return (MatchPage m replayText)
        _ -> error "match id collision, this should never happen"

replay :: MonadIO m => StateVar -> MatchId -> m ReplayText
replay stateVar mid = do
    ServerState _ _ matches <- readStateVar stateVar
    -- TODO: better than O(n) lookup
    case V.filter ((== mid) . matchId) matches of
        [] -> error ("no match with " <> show mid)
        [m] -> do
            ReplayText <$> liftIO (TLIO.readFile (toFilePath (matchReplayPath m)))
        _ -> error "match id collision, this should never happen"

mainPage :: MonadIO m => StateVar -> m MainPage
mainPage stateVar = MainPage <$> readStateVar stateVar

help :: Monad m => m APIDocs
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
        return (ServerState (MatchId (V.length matches)) bots matches)

instance ToSample Bot Bot where
    toSample _ = Bot "Randy" . ExecutableBot <$> parseAbsFile "/home/randy/src/shiny_metal_bot.sh"

instance ToSample MainPage MainPage where
    toSample _ = MainPage <$> toSample (Proxy :: Proxy ServerState)

instance ToSample MatchPage MatchPage where
    toSample _ = Nothing

instance ToCapture (Capture "matchId" MatchId) where
    toCapture _ = DocCapture "matchId" "Integer id of the match"

instance ToSample ServerStateUpdate ServerStateUpdate where
    toSample _ = Nothing
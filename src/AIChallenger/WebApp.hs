{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger.WebApp
    ( webApp
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Vector as V
import qualified Network.Wai as Wai
import Network.Wai.Metrics
import Network.Wai.Middleware.RequestLogger
import Path
import Servant
import Servant.HTML.Lucid

import AIChallenger.HTML
import AIChallenger.Match
import AIChallenger.StateVar
import AIChallenger.Types

turnLimit = Turn 100

type WebAPI
    = Get '[HTML] MainPage
    :<|> "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot
    :<|> "launch-tournament" :> Post '[PlainText] T.Text
    :<|> "match" :> Capture "matchId" MatchId :> Get '[HTML] MatchPage
    :<|> "help" :> Get '[HTML] T.Text

webApp :: Game game => game -> StateVar -> WaiMetrics -> Wai.Application
webApp game stateVar waiMetrics = do
    let handlers = mainPage stateVar
            :<|> readStateVar stateVar
            :<|> postBot stateVar
            :<|> launchTournament game stateVar
            :<|> replay stateVar
            :<|> help
    let middleware :: [Wai.Application -> Wai.Application]
        middleware = [metrics waiMetrics, logStdout]
    foldr ($) (serve (Proxy :: Proxy WebAPI) handlers) middleware

postBot :: MonadIO m => StateVar -> Bot -> m Bot
postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    _ <- modifyStateVar stateVar (AddBot bot)
    return bot

launchTournament :: (Game game, MonadIO m) => game -> StateVar -> m T.Text
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
    return (T.pack (show (length pairs) <> " matches scheduled"))

replay :: MonadIO m => StateVar -> MatchId -> m MatchPage
replay stateVar mid = do
    ServerState _ _ matches <- readStateVar stateVar
    -- TODO: better than O(n) lookup
    case V.filter ((== mid) . matchId) matches of
        [] -> error ("no match with " <> show mid)
        [match] -> do
            replayText <- liftIO (TLIO.readFile (toFilePath (matchReplayPath match)))
            return (MatchPage match replayText)
        _ -> error "match id collision, this should never happen"

mainPage :: MonadIO m => StateVar -> m MainPage
mainPage stateVar = MainPage <$> readStateVar stateVar

help = error "no help yet"
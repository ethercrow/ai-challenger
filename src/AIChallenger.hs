{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger
    ( startJudge
    ) where

import Control.Concurrent
import Control.Monad.Trans
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Path
import Servant
import qualified System.Remote.Monitoring as EKG

import AIChallenger.Match
import AIChallenger.StateVar
import AIChallenger.Types

type WebAPI
    = "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot
    :<|> "launch-tournament" :> Post '[PlainText] T.Text

startJudge :: Game game => game -> IO ()
startJudge game = do
    _ <- EKG.forkServer "localhost" 7999
    stateVar <- mkStateVar
    _ <- modifyStateVar stateVar (AddBot (Bot "bender" (ExecutableBot $(mkAbsFile "/home/ethercrow/src/ai-challenger/demo/rock.py"))))
    Warp.run 8081 (app game stateVar)

app :: Game game => game -> StateVar -> Wai.Application
app game stateVar =
    let handlers = readStateVar stateVar :<|> postBot stateVar :<|> launchTournament game stateVar
    in serve (Proxy :: Proxy WebAPI) handlers

postBot :: MonadIO m => StateVar -> Bot -> m Bot
postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    _ <- modifyStateVar stateVar (AddBot bot)
    return bot

launchTournament :: (Game game, MonadIO m) => game -> StateVar -> m T.Text
launchTournament game stateVar = do
    ServerState bots _ <- readStateVar stateVar
    let pairs = [V.fromList [b1, b2] | b1 <- bots, b2 <- bots, b1 < b2]
    _ <- liftIO . forkIO $ do
        V.forM_ pairs $ \pair -> do
            result <- launchBotsAndSimulateMatch game (Turn 100) pair
            _ <- modifyStateVar stateVar (AddMatch result)
            return ()
    return (T.pack (show (length pairs) <> " matches scheduled"))
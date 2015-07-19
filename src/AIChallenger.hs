{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger
    ( startJudge
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad.Trans
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import qualified System.Remote.Monitoring as EKG

import AIChallenger.Bot
import AIChallenger.Match
import AIChallenger.StateVar
import AIChallenger.Types

type WebAPI
    = "state" :> Get '[JSON] ServerState
    :<|> "add-bot" :> ReqBody '[JSON] Bot :> Post '[JSON] Bot

startJudge :: Game game => game -> IO ()
startJudge _game = do
    _ <- EKG.forkServer "localhost" 7999
    stateVar <- mkStateVar
    Warp.run 8081 (app stateVar)

app :: StateVar -> Wai.Application
app stateVar =
    let handlers = readStateVar stateVar :<|> postBot stateVar
    in serve (Proxy :: Proxy WebAPI) handlers

postBot stateVar bot = do
    liftIO (putStrLn ("Adding bot: " <> show bot))
    modifyStateVar stateVar (AddBot bot)
    return bot

launchBotsAndSimulateMatch :: Game game => game -> Turn -> [FilePath] -> IO ()
launchBotsAndSimulateMatch game turnLimit exes = do
    bracket (launchBots exes) (mapM_ playerClose) $ \bots -> do
        GameResult winners _ _ <- simulateMatch game turnLimit bots
        TIO.putStrLn ("Winners: " <> T.intercalate ", "
            (map playerName (filter ((`elem` winners) . playerId) bots)))
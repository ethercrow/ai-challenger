{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger
    ( startJudge
    ) where

import Control.Exception
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant

import AIChallenger.Bot
import AIChallenger.Types
import AIChallenger.Match

type HTTPAPI = "ohai" :> Get '[PlainText] T.Text

startJudge :: Game game => game -> IO ()
startJudge _game = Warp.run 8081 app

app :: Wai.Application
app = serve (Proxy :: Proxy HTTPAPI) (return "OHAI")

launchBotsAndSimulateMatch :: Game game => game -> Turn -> [FilePath] -> IO ()
launchBotsAndSimulateMatch game turnLimit exes = do
    bracket (launchBots exes) (mapM_ botClose) $ \bots -> do
        GameResult winners _ _ <- simulateMatch game turnLimit bots
        TIO.putStrLn ("Winners: " <> T.intercalate ", "
            (map botName (filter ((`elem` winners) . botId) bots)))
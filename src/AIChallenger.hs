{-# LANGUAGE OverloadedStrings #-}

module AIChallenger
    ( startJudge
    ) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Remote.Monitoring as EKG
import Path

import AIChallenger.StateVar
import AIChallenger.Types
import AIChallenger.WebApp

startJudge :: Game game => game -> [Path Abs File] -> IO ()
startJudge game exes = do
    _ <- EKG.forkServer "localhost" 7999
    stateVar <- mkStateVar
    let bots = [Bot (showT (filename exe)) (ExecutableBot exe) | exe <- exes]
    mapM_ (modifyStateVar stateVar . AddBot) bots
    Warp.run 8081 (webApp game stateVar)

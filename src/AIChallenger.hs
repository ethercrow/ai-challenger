{-# LANGUAGE OverloadedStrings #-}

module AIChallenger
    ( startJudge
    ) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Remote.Monitoring as EKG

import AIChallenger.StateVar
import AIChallenger.Types
import AIChallenger.WebApp

startJudge :: Game game => game -> IO ()
startJudge game = do
    _ <- EKG.forkServer "localhost" 7999
    stateVar <- mkStateVar
    Warp.run 8081 (webApp game stateVar)

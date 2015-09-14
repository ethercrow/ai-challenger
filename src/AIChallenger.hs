{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AIChallenger
    ( startJudge
    ) where

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Metrics
import qualified System.Remote.Monitoring as EKG
import Path
import System.Environment

import AIChallenger.StateVar
import AIChallenger.Types
import AIChallenger.WebApp

startJudge :: Game game => game -> [Path Abs File] -> IO ()
startJudge game exes = do
    metricStore <- EKG.serverMetricStore <$> EKG.forkServer "localhost" 7999
    webMetrics <- registerWaiMetrics metricStore
    stateVar <- mkStateVar
    let bots = [Bot (showT (filename exe)) (ExecutableBot exe) | exe <- exes]
    mapM_ (modifyStateVar stateVar . AddBot) bots
    selfPath <- parseAbsFile =<< getExecutablePath
    let dashboardDir = parent selfPath </> $(mkRelDir "dashboard")
    Warp.run 8081 (webApp game stateVar webMetrics dashboardDir)

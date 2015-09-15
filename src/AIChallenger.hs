{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AIChallenger
    ( startJudge
    , Config(..)
    , getConfigFromCommandlineFlags
    ) where

import Data.Function
import Data.String
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Metrics
import qualified System.Remote.Monitoring as EKG
import Path
import System.Environment

import AIChallenger.Config
import AIChallenger.StateVar
import AIChallenger.Types
import AIChallenger.WebApp

startJudge :: Game game => game -> Config -> IO ()
startJudge game config = do
    metricStore <- EKG.serverMetricStore <$> EKG.forkServer "localhost" 7999
    webMetrics <- registerWaiMetrics metricStore
    stateVar <- mkStateVar
    let bots =
            [ Bot (T.pack (toFilePath (filename exe))) (ExecutableBot exe)
            | exe <- cfgBotExecutables config
            ]
    mapM_ (modifyStateVar stateVar . AddBot) bots
    selfPath <- parseAbsFile =<< getExecutablePath
    let dashboardDir = parent selfPath </> $(mkRelDir "dashboard")
        settings = Warp.defaultSettings
            & Warp.setPort (cfgPort config)
            & Warp.setHost (fromString (cfgAddress config))
    Warp.runSettings settings (webApp game stateVar webMetrics dashboardDir)

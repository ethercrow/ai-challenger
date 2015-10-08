{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AIChallenger
    ( startJudge
    , Config(..)
    , getConfigFromCommandlineFlags
    ) where

import Data.Function
import Data.Monoid
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
    metricStore <- EKG.serverMetricStore <$> EKG.forkServer "127.0.0.1" 7999
    webMetrics <- registerWaiMetrics metricStore
    stateVar <- mkStateVar
    let bots =
            [ Bot (T.pack (toFilePath (filename exe))) (ExecutableBot exe)
            | exe <- cfgBotExecutables config
            ]
    mapM_ (\bot -> addBot bot stateVar) bots
    selfPath <- parseAbsFile =<< getExecutablePath
    let settings = Warp.defaultSettings
            & Warp.setPort (cfgPort config)
            & Warp.setHost (fromString (cfgAddress config))
    putStrLn ("Dashboard is at http://" <> cfgAddress config
        <> ":" <> show (cfgPort config))
    putStrLn "EKG is at http://127.0.0.1:7999"
    Warp.runSettings settings (webApp game stateVar (Just webMetrics))

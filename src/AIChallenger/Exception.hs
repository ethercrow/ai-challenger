{-# LANGUAGE ScopedTypeVariables #-}

module AIChallenger.Exception
    ( catchAll
    ) where

import Control.Exception.Base
import Data.Monoid
import System.IO

catchAll :: IO () -> IO ()
catchAll action = do
    catch action
        (\e -> do
            -- TODO: include call stack when 7.10.2 is available
            hPutStrLn stderr ("catchAll: " <> show (e :: SomeException))
            return ())
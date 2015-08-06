{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}

module AIChallenger.Exception
    ( catchAll
    ) where

import Control.Exception.Base
import GHC.Stack
import Data.Monoid
import System.IO

catchAll :: (?loc :: CallStack) => IO () -> IO ()
catchAll action = do
    catch action
        (\e -> do
            hPutStrLn stderr ("catchAll: " <> show (e :: SomeException))
            hPutStrLn stderr ("call stack: " <> showCallStack ?loc)
            return ())
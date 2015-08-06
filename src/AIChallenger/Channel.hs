{-# LANGUAGE OverloadedStrings #-}

module AIChallenger.Channel where

import Control.Exception
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO

import AIChallenger.Exception
import AIChallenger.Types

data OutChannel = OutChannel
    { sendLine :: T.Text -> IO (Either Fault ())
    , closeOutChannel :: IO ()
    }

data InChannel = InChannel
    { receiveLine :: IO (Either Fault T.Text)
    , channelEOF :: IO Bool
    , closeInChannel :: IO ()
    }

inChannelFromHandle :: Handle -> InChannel
inChannelFromHandle h = InChannel
    (catch
        (Right <$> TIO.hGetLine h)
        (\e ->
            let msg = "receiveLine(" <> showT h <> "): " <> showT (e :: SomeException)
            in return (Left (Fault msg))))
    (hIsEOF h)
    (catchAll (hClose h))

stdinChannel :: InChannel
stdinChannel = inChannelFromHandle stdin

yesChannel :: T.Text -> InChannel
yesChannel line = InChannel (return (Right line)) (return False) (return ())

outChannelFromHandle :: Handle -> OutChannel
outChannelFromHandle h = OutChannel
    (\ln ->
        catch
            (do
                TIO.hPutStrLn h ln
                hFlush h
                return (Right ()))
            (\e ->
                let msg = "sendLine(" <> showT h <> "): " <> showT (e :: SomeException)
                in return (Left (Fault msg))))
    (catchAll (hClose h))

stdoutChannel :: OutChannel
stdoutChannel = outChannelFromHandle stdout

whateverChannel :: OutChannel
whateverChannel = OutChannel (const (return (Right ()))) (return ())
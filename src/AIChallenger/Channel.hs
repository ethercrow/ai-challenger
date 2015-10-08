{-# LANGUAGE OverloadedStrings #-}

module AIChallenger.Channel where

import Control.Exception
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.WebSockets
import System.IO

import AIChallenger.Exception
import AIChallenger.Types

inChannelFromHandle :: Handle -> InChannel
inChannelFromHandle h = InChannel
    (catch
        (Right <$> TIO.hGetLine h)
        (\e ->
            let msg = "receiveLine(" <> showT h <> "): " <> showT (e :: SomeException)
            in return (Left (Fault msg))))
    (hIsEOF h)
    (do -- print ("closing handle " <> show h)
        catchAll (hClose h))

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
    (do -- print ("closing handle " <> show h)
        catchAll (hClose h))

inWSChannel :: Connection -> InChannel
inWSChannel conn = InChannel
    (catch
        (do
            t <- receiveData conn
            return (Right t))
        (\e ->
            let msg = "receiveLine(websocket): " <> showT (e :: SomeException)
            in return (Left (Fault msg))))
    (return False)
    (return ())

outWSChannel :: Connection -> OutChannel
outWSChannel conn = OutChannel
    (\ln ->
        catch
            (do
                sendTextData conn ln
                return (Right ()))
            (\e ->
                let msg = "sendLine(websocket): " <> showT (e :: SomeException)
                in return (Left (Fault msg))))
    (return ())

stdoutChannel :: OutChannel
stdoutChannel = outChannelFromHandle stdout

whateverChannel :: OutChannel
whateverChannel = OutChannel (const (return (Right ()))) (return ())

chReadLinesUntilDot :: InChannel -> IO (Either Fault [T.Text])
chReadLinesUntilDot ch = do
    fmap (fmap reverse) (go [])
    where
    go acc = do
        faultOrLine <- receiveLine ch
        case faultOrLine of
          Right "." -> return (Right acc)
          Right l -> go (l : acc)
          Left fault -> return (Left fault)
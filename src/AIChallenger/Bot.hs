{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AIChallenger.Bot
    ( Player (..)
    , launchBots
    ) where

import Control.Concurrent
import Control.Exception (SomeException, catch)
import Control.Monad
import Data.Char (isDigit)
import Data.Monoid
import qualified Data.Text as T
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as Socket
import System.Exit
import System.IO
import System.Process

import AIChallenger.Types
import AIChallenger.Channel

data Player = Player
    { playerId :: !PlayerId
    , playerName :: !T.Text
    , playerInput :: !OutChannel
    , playerOutput :: !InChannel
    , playerClose :: IO ()
    }

launchBots :: [FilePath] -> IO [Player]
launchBots = mapM launch . zip [1..]
    where
    launch (index, "builtin-idle") =
        return
            (Player
                (PlayerId index)
                "builtin-idle"
                whateverChannel
                (yesChannel ".")
                (return ()))
    launch (index, port) | all isDigit port = do
        handleVar <- newEmptyMVar
        finishVar <- newEmptyMVar
        _ <- TCP.listen TCP.HostAny port $ \(listeningSocket, listeningAddr) -> do
            putStrLn $ "Listening for incoming connections at " ++ show listeningAddr
            TCP.acceptFork listeningSocket $ \(connectionSocket, remoteAddr) -> do
                putStrLn $ "Connection established from " ++ show remoteAddr
                hFlush stdout
                h <- Socket.socketToHandle connectionSocket ReadWriteMode
                putMVar handleVar h
                void (takeMVar finishVar)
            putStrLn "3"
            hFlush stdout
        putStrLn "waiting for handle to appear"
        h <- takeMVar handleVar
        return
            (Player
                (PlayerId index)
                ("port-" <> T.pack port)
                (outChannelFromHandle h)
                (inChannelFromHandle h)
                (hClose h >> putMVar finishVar ()))
    launch (index, path) = do
        handles <-
            createProcess
                (proc path [])
                    { std_out = CreatePipe
                    , std_in = CreatePipe
                    , create_group = True
                    }
        case handles of
            (Just hIn, Just hOut, _, procHandle) -> do
                hSetBuffering hOut LineBuffering
                hSetBuffering hIn LineBuffering
                return
                    (Player
                        (PlayerId index)
                        (T.pack path)
                        (outChannelFromHandle hIn)
                        (inChannelFromHandle hOut)
                        (catch
                            (terminateProcess procHandle)
                            (\(_ :: SomeException) -> return ())))
            _ -> do 
                print ("failed to start " ++ path)
                exitWith (ExitFailure 10)
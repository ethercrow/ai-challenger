{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module AIChallenger.Bot
    ( Player (..)
    , playerClose
    , launchBots
    ) where

import Control.Concurrent
import Control.Exception (SomeException, catch)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as Socket
import System.Exit
import System.IO
import System.Process
import qualified Path as P

import AIChallenger.Types
import AIChallenger.Channel

data Player = Player
    { playerId :: !PlayerId
    , playerName :: !T.Text
    , playerInput :: !OutChannel
    , playerOutput :: !InChannel
    , playerAdditionalShutdown :: IO ()
    }

playerClose :: Player -> IO ()
playerClose (Player _ _ input output shutdown) = do
    closeInChannel output
    closeOutChannel input
    shutdown

launchBots :: V.Vector Bot -> IO (V.Vector Player)
launchBots bots = mapM launch (V.zip [1 .. V.length bots] bots)
    where
    launch (index, Bot name IdleBot) =
        return
            (Player
                (PlayerId index)
                name 
                whateverChannel
                (yesChannel ".")
                (return ()))

    launch (index, Bot name (TCPBot port)) = do
        handleVar <- newEmptyMVar
        finishVar <- newEmptyMVar
        _ <- TCP.listen TCP.HostAny (show port) $ \(listeningSocket, listeningAddr) -> do
            putStrLn $ "Listening for incoming connections at " ++ show listeningAddr
            _ <- TCP.acceptFork listeningSocket $ \(connectionSocket, remoteAddr) -> do
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
                name
                (outChannelFromHandle h)
                (inChannelFromHandle h)
                (hClose h >> putMVar finishVar ()))

    launch (index, Bot name (ExecutableBot path)) = do
        handles <-
            createProcess
                (proc (P.toFilePath path) [])
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
                        name
                        (outChannelFromHandle hIn)
                        (inChannelFromHandle hOut)
                        (catch
                            (terminateProcess procHandle)
                            (\(_ :: SomeException) -> return ())))
            _ -> do 
                print ("failed to start " ++ P.toFilePath path)
                exitWith (ExitFailure 10)
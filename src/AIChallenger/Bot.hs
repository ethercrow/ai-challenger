{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module AIChallenger.Bot
    ( Player (..)
    , playerClose
    , launchBots
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as Socket
import System.Exit
import System.IO
import System.Process
import qualified Path as P

import AIChallenger.Channel
import AIChallenger.Exception
import AIChallenger.Types

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

launchBots :: V.Vector Bot -> IO (V.Vector (Either Fault Player))
launchBots bots = mapM launch (V.zip [1 .. V.length bots] bots)
    where
    launch (index, Bot name IdleBot) =
        return $ Right
            (Player
                (PlayerId index)
                name 
                whateverChannel
                (yesChannel ".")
                (return ()))

    launch (index, Bot name (TCPBot port)) = do
        hopefullyPlayer <- try $ do
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
        case hopefullyPlayer of
            Right p -> return (Right p)
            Left e ->
                let msg = "Failed to launch bot '" <> name <> "': "
                        <> T.pack (show (e :: SomeException))
                in return (Left (Fault msg))

    launch (index, Bot name (ExecutableBot path)) = do
        playerOrException <- try $ do
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
                            (catchAll (terminateProcess procHandle)))
                _ -> do 
                    putStrLn ("failed to start " <> P.toFilePath path)
                    exitWith (ExitFailure 10)
        case playerOrException of
            Right p -> return (Right p)
            Left e ->
                let msg = "Failed to launch bot '" <> name <> "': "
                        <> T.pack (show (e :: SomeException))
                in return (Left (Fault msg))

instance Show Player where
    show (Player pid name _inCh _outCh _close) =
        show pid <> " " <> T.unpack name

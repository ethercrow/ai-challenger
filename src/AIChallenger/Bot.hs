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
import Control.Monad.Extra
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector.Extended as V
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as Socket
import System.Directory
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
playerClose (Player _ name input output shutdown) = do
    print ("playerClose " <> name)
    catchAll (closeInChannel output)
    catchAll (closeOutChannel input)
    catchAll shutdown

launchBots :: V.Vector Bot -> IO (Either (V.Vector Bot, Faults) (V.Vector Player))
launchBots bots = do
    let playerIds = fmap PlayerId [1 .. V.length bots]
    playersAndFaults <- mapM launch (V.zip playerIds bots)
    let players = V.mapMaybe (either (const Nothing) Just) playersAndFaults
    if V.length players == V.length playersAndFaults
    then return (Right players)
    else do
        mapM_ playerClose players
        let faults = V.mapMaybe (either Just (const Nothing)) playersAndFaults
            winnerNames = fmap playerName players
            winnerBots = V.filter ((`elem` winnerNames). botName) bots
        return (Left (winnerBots, faults))
    where
    launch (pid, Bot name IdleBot) =
        return $ Right
            (Player
                pid
                name 
                whateverChannel
                (yesChannel ".")
                (return ()))

    launch (pid, Bot name (TCPBot port)) = do
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
                    pid
                    name
                    (outChannelFromHandle h)
                    (inChannelFromHandle h)
                    (hClose h >> putMVar finishVar ()))
        case hopefullyPlayer of
            Right p -> return (Right p)
            Left e ->
                let msg = "Failed to launch bot '" <> name <> "': "
                        <> T.pack (show (e :: SomeException))
                in return (Left (pid, pure (Fault msg)))

    launch (pid, Bot name (ExecutableBot path)) = do
        playerOrFaultOrException <- try $ do
            let exeFilePath = P.toFilePath path

            fileExists <- doesFileExist exeFilePath
            if not fileExists
            then return (Left (Fault (T.pack exeFilePath <> " does not exist")))
            else return (Right ())
                
            isExecutable <- executable <$> getPermissions exeFilePath
            if not isExecutable 
            then return (Left (Fault (T.pack exeFilePath <> " is not executable")))
            else return (Right ())
            
            (Just hIn, Just hOut, _, procHandle) <-
                createProcess
                    (proc exeFilePath [])
                        { std_out = CreatePipe
                        , std_in = CreatePipe
                        , close_fds = True
                        , create_group = True
                        }
            hSetBuffering hOut LineBuffering
            hSetBuffering hIn LineBuffering
            return (Right (Player
                    pid
                    name
                    (outChannelFromHandle hIn)
                    (inChannelFromHandle hOut)
                    (do (catchAll (terminateProcess procHandle))
                        (catchAll (void $ waitForProcess procHandle)))))
        case playerOrFaultOrException of
            Right (Right p) -> return (Right p)
            Right (Left fault) ->
                return (Left (pid, pure fault))
            Left e -> do
                let msg = "Failed to launch bot '" <> name <> "': "
                        <> T.pack (show (e :: SomeException))
                print msg
                return (Left (pid, pure (Fault msg)))

instance Show Player where
    show (Player pid name _inCh _outCh _close) =
        show pid <> " " <> T.unpack name

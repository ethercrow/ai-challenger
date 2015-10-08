{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Options.Applicative
import Path
import System.Directory
import System.Process hiding (cwd)
import System.Exit
import Network.WebSockets

import AIChallenger.Channel
import AIChallenger.Exception
import AIChallenger.Types

main :: IO ()
main = do
    cfg <- getConfigFromCommandlineFlags
    runClient
        (cfgServerHost cfg)
        (cfgServerPort cfg)
        ("/play/" <> cfgName cfg)
        (app (toFilePath (cfgLocalExecutable cfg)))

app :: FilePath -> Connection -> IO ()
app exe conn = waitForGame
    where
    waitForGame = do
        putStrLn "Waiting for a game"
        msg <- receiveDataMessage conn
        case msg of
            Text "GameStart" -> do
                (Just hIn, Just hOut, _, procHandle) <-
                    createProcess
                        (proc exe [])
                            { std_out = CreatePipe
                            , std_in = CreatePipe
                            , close_fds = True
                            , create_group = True
                            }
                putStrLn "Game started"
                play (inChannelFromHandle hOut) (outChannelFromHandle hIn)
                putStrLn "Game finished"
                catchAll (terminateProcess procHandle)
                catchAll (void $ waitForProcess procHandle)
                waitForGame
            _ -> die ("Unexpected message from server: " <> show msg)
    play fromBot toBot =
        let go = do
                msg <- receiveDataMessage conn
                case msg of
                    Text "GameOver" -> return ()
                    Text "." -> do
                        Right () <- sendLine toBot "."
                        Right orders <- chReadLinesUntilDot fromBot
                        mapM_ (sendTextData conn) orders
                        sendDataMessage conn (Text ".")
                        go
                    Text x -> do
                        Right () <- sendLine toBot (TL.toStrict (TLE.decodeUtf8 x))
                        go
                    _ -> die ("Unexpected message from server: " <> show msg)
        in go

data Config = Config
    { cfgServerHost :: String
    , cfgServerPort :: Int
    , cfgName :: String
    , cfgLocalExecutable :: Path Abs File
    } deriving Show

getConfigFromCommandlineFlags :: IO Config
getConfigFromCommandlineFlags = do
    cwd <- parseAbsDir =<< getCurrentDirectory
    let parseFileName fn = parseAbsFile fn <|> fmap (cwd </>) (parseRelFile fn)
    config <- execParser (info (parseConfig parseFileName) (progDesc "ai-challenger-remote-play"))
    let exe = toFilePath (cfgLocalExecutable config)
    isExecutable <- executable <$> getPermissions exe
    when (not isExecutable) $
        die (exe <> " is not an executable file")
    return config

parseConfig :: (String -> Maybe (Path Abs File)) -> Parser Config
parseConfig parseFileName = Config
    <$> strOption (long "host")
    <*> option auto (long "port")
    <*> strOption (long "name")
    <*> fmap
        (fromMaybe (error "bad executable name") . parseFileName)
        (strOption (long "executable"))
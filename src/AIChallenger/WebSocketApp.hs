{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger.WebSocketApp
    ( wsApp
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.WebSockets
import Text.Read (readMaybe)

import AIChallenger.StateVar
import AIChallenger.Types

wsSendJSON :: A.ToJSON a => Connection -> a -> IO ()
wsSendJSON conn x = sendDataMessage conn (Text (A.encode x))

wsApp :: StateVar -> ServerApp
wsApp stateVar pendingConnection = do
    (state, chan) <- subscribeToStateUpdates stateVar
    let reqPath = requestPath (pendingRequest pendingConnection)
    case reqPath of
        "state" -> do
            conn <- acceptRequest pendingConnection
            BS.putStrLn ("Incoming websocket request " <> reqPath)
            wsSendJSON conn state
            forever $ do
                update <- readChan chan
                wsSendJSON conn update
        "/tournaments" -> do
            conn <- acceptRequest pendingConnection
            mapM_ (wsSendJSON conn) (ssTournaments state)
            forever $ do
                update <- readChan chan
                case update of
                    AddTournament t -> wsSendJSON conn t
                    _ -> return ()
        (BS.split '/' -> ["", "play", nameBS]) -> do
            let name = TE.decodeUtf8 nameBS
            void $ addBot (Bot name WebSocketBot) stateVar
            conn <- acceptRequest pendingConnection
            void $ addWebSocketConnection name conn stateVar
            TIO.putStrLn ("Added websocket bot " <> name)
            let go = do
                    threadDelay 1000000
                    catch
                        (sendPing conn ("PING" :: BS.ByteString) >> go)
                        (\e -> do
                            void $ removeWebSocketBot name stateVar
                            print (e :: SomeException))
            go
        (BS.split '/' -> ["", "tournament", (fmap TournamentId . readMaybe . BS.unpack) -> Just tid]) -> do
            -- TODO: better lookup performance
            case V.filter ((== tid) . tId) (ssTournaments state) of
                (V.null -> True) -> do
                    BS.putStrLn ("websocket request " <> reqPath <> " rejected, because 404")
                    rejectRequest pendingConnection (BS.pack ("no tournament with " <> show tid))
                (V.toList -> [t]) -> do
                    conn <- acceptRequest pendingConnection
                    let mids = S.fromList (V.toList (tMatchIds t))
                        completedMatches = V.filter ((`S.member` mids) . matchId) (ssMatches state)
                        remainingMids = mids `S.difference` (S.fromList (V.toList (V.map matchId completedMatches)))
                    wsSendJSON conn t
                    mapM_ (wsSendJSON conn) completedMatches
                    let go (S.null -> True) = return ()
                        go stillRemaining = do
                            update <- readChan chan
                            case update of
                                AddMatch m | matchId m `S.member` stillRemaining -> do
                                    wsSendJSON conn m
                                    go (S.delete (matchId m) stillRemaining)
                                _ -> return ()
                    go remainingMids
                _ -> error "tournament id collision, this should never happen"
        _ -> do
            BS.putStrLn ("Rejected unknown websocket request " <> reqPath)
            rejectRequest pendingConnection ("Unknown path " <> reqPath)
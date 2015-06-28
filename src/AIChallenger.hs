{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger
    ( startJudge
    , Turn (..)
    ) where

import Control.Exception
import Control.Monad
import Data.List ((\\))
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import AIChallenger.Bot
import AIChallenger.Channel
import AIChallenger.Types

startJudge :: Game game => game-> Turn  -> [FilePath] -> IO ()
startJudge game turnLimit exes = do
    bots <- launchBots exes
    mapM_ (TIO.putStrLn . botName) bots
    result <- simulateMatch game bots turnLimit
    print result
    mapM_ botClose bots

simulateMatch :: Game game => game -> [Bot] -> Turn -> IO (GameResult game)
simulateMatch game bots turnLimit =
    go mempty (gameInitialState game)
    where
    go turn state | turn > turnLimit = return (gameTimeout state)
    go turn state = do
        sendWorld game state bots
        faultsOrOrders <- getOrders game bots
        case faultsOrOrders of
            Left faults -> do
                let winners = fmap botId bots \\ M.keys faults
                return (GameResult
                    winners
                    (Disqualification faults)
                    (gameExtractReplay game state))
            Right orders ->
                case gameAdvance orders state of
                    Left result -> return result
                    Right newState -> go (succ turn) newState

sendWorld :: Game game => game -> GameState game -> [Bot] -> IO ()
sendWorld _ gameState =
    mapM_ $ \(Bot { botId = (PlayerId me), botInput = ch }) -> do
        sendLine ch "."

getOrders :: Game game => game -> [Bot]
    -> IO (Either Faults (M.Map PlayerId [GameOrder game]))
getOrders game bots = do
    unparsedOrdersAndFaults <- forM bots $ \(Bot {botId = me, botOutput = ch}) ->
        do
            faultOrTexts <- chReadLinesUntilDot ch
            case faultOrTexts of
                Left fault -> return (me, Left (pure fault))
                Right texts -> return (me, Right texts)
    let ioFaults =
            [ (botIdent, f)
            | (botIdent, Left f) <- unparsedOrdersAndFaults
            ]
        unparsedOrders =
            [ (botIdent, ts)
            | (botIdent, Right ts) <- unparsedOrdersAndFaults
            ]
        badOrders =
            mapMaybe
                (sequence . fmap
                    (\os -> nonEmpty
                        [ Fault ("Failed to parse order " <> t)
                        | t@(gameParseOrder game -> Nothing) <- os
                        ]))
                unparsedOrders
        goodOrders =
            fmap
                (fmap (\os -> [o | (gameParseOrder game -> Just o) <- os]))
                unparsedOrders
    case (ioFaults, badOrders, goodOrders) of
        ([], [], _) -> return (Right (M.fromList goodOrders))
        _ -> return (Left (M.fromList (ioFaults <> badOrders)))

chReadLinesUntilDot :: InChannel -> IO (Either Fault [T.Text])
chReadLinesUntilDot ch = do
    linesOrException <- try (fmap reverse (go []))
    return $ case linesOrException of
        Left (SomeException e) -> Left (Fault (T.pack (show e)))
        Right x -> Right x
    where
    go acc = do
        l <- receiveLine ch
        case l of
          "." -> return acc
          _ -> go (l : acc)
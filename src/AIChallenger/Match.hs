{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger.Match
    ( launchBotsAndSimulateMatch
    , simulateMatch
    ) where

import Control.Exception
import Control.Monad
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector.Extended as V
import Path
import System.Timeout

import AIChallenger.Bot
import AIChallenger.Types

launchBotsAndSimulateMatch :: Game game => game -> Turn -> V.Vector Bot -> TournamentId -> MatchId -> IO Match
launchBotsAndSimulateMatch game turnLimit bots tournamentId mid@(MatchId x) = do
    replayPath <- parseAbsFile ("/tmp/" <> show x <> ".replay")
    let work (Right players) = do
             GameResult winnerIds gameOver replay <- simulateMatch game turnLimit players
             gameSaveReplay game replayPath replay
             let winnerPlayers = (V.filter ((`elem` winnerIds) . playerId) players)
             let winnerNames = fmap playerName winnerPlayers
                 winnerBots = V.filter ((`elem` winnerNames) .  botName) bots
             return (Match mid tournamentId bots winnerBots gameOver replayPath)
        work (Left (winnerBots, faults)) = do
             let replay = gameExtractReplay game (gameInitialState game)
             gameSaveReplay game replayPath replay
             return (Match mid tournamentId bots winnerBots (Disqualification faults) replayPath)
    bracket
        (launchBots bots)
        (either (const (return ())) (V.mapM_ playerClose))
        work

simulateMatch :: Game game => game -> Turn -> V.Vector Player -> IO (GameResult game)
simulateMatch game turnLimit bots =
    go mempty (gameInitialState game)
    where
    go turn state | turn >= turnLimit = return (gameTimeout state)
    go turn state = do
        sendWorld game state bots
        faultsOrOrders <- getOrders game bots
        case faultsOrOrders of
            Left faults -> do
                let losers = fmap fst faults
                    winners = V.filter (not . (`V.elem` losers)) (fmap playerId bots)
                return (GameResult
                    winners
                    (Disqualification faults)
                    (gameExtractReplay game state))
            Right orders ->
                case gameAdvance orders state of
                    Left result -> return result
                    Right newState -> go (succ turn) newState

sendWorld :: Game game => game -> GameState game -> V.Vector Player -> IO ()
sendWorld game gameState players = do
    forM_ players $ \(Player { playerId = PlayerId me, playerInput = ch }) -> do
        _ <- sendLine ch ("Y " <> showT me)
        gameSendWorld game (PlayerId me) gameState (sendLine ch)
        sendLine ch "."

getOrders :: forall game. Game game => game -> V.Vector Player
    -> IO (Either Faults (V.Vector (PlayerId, V.Vector (GameOrder game))))
getOrders game bots = do
    unparsedOrdersAndFaults <- forM bots $ \(Player {playerId = me, playerOutput = ch}) ->
        do
            let oneSecond = 1000000
            maybeFaultOrTexts <- timeout oneSecond (chReadLinesUntilDot ch)
            case maybeFaultOrTexts of
                Nothing -> return (me, Left (pure (Fault "Time limit exceeded")))
                Just (Left fault) -> return (me, Left (pure fault))
                Just (Right texts) -> return (me, Right texts)
    let ioFaults :: V.Vector (PlayerId, NonEmpty Fault)
        ioFaults =
            V.mapMaybe
                (\case
                    (botIdent, Left f) -> Just (botIdent, f)
                    _ -> Nothing)
                unparsedOrdersAndFaults
        unparsedOrders :: V.Vector (PlayerId, V.Vector T.Text)
        unparsedOrders =
            V.mapMaybe
                (\case
                    (botIdent, Right ts) -> Just (botIdent, V.fromList ts)
                    _ -> Nothing)
                unparsedOrdersAndFaults
        badOrders :: V.Vector (PlayerId, NonEmpty Fault)
        badOrders =
            V.mapMaybe
                (sequence . fmap
                    (\os -> nonEmpty
                        [ Fault ("Failed to parse order " <> t)
                        | t@(gameParseOrder game -> Nothing) <- V.toList os
                        ]))
                unparsedOrders
        goodOrders :: V.Vector (PlayerId, V.Vector (GameOrder game))
        goodOrders =
            fmap
                (fmap (\os -> 
                    V.mapMaybe
                        (\case
                            (gameParseOrder game -> Just o) -> Just o
                            _ -> Nothing)
                        os))
                unparsedOrders
    if V.null ioFaults && V.null badOrders
    then return (Right goodOrders)
    else return (Left (ioFaults <> badOrders))

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
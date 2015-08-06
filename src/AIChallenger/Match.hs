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
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V

import AIChallenger.Bot
import AIChallenger.Channel
import AIChallenger.Types

launchBotsAndSimulateMatch :: Game game => game -> Turn -> V.Vector Bot -> MatchId -> IO Match
launchBotsAndSimulateMatch game turnLimit bots mid =
    bracket
        (launchBots bots)
        (mapM_ (either (const (return ())) playerClose))
        (\playersOrFaults -> do
            let players = vectorMapMaybe (either (const Nothing) Just) playersOrFaults
            if V.length players == V.length playersOrFaults
            then do
                GameResult winnerIds gameOver _ <- simulateMatch game turnLimit players
                let winnerPlayers = (V.filter ((`elem` winnerIds) . playerId) players)
                let winnerNames = fmap playerName winnerPlayers
                    winnerBots = V.filter ((`elem` winnerNames) .  botName) bots
                return (Match mid bots winnerBots gameOver)
            else do
                let winnerNames = fmap playerName players
                    winnerBots = V.filter ((`elem` winnerNames) .  botName) bots
                return (Match mid bots winnerBots (Disqualification mempty)))

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
sendWorld _ gameState players = do
    forM_ players $ \(Player { playerId = PlayerId me, playerInput = ch }) -> do
        -- TODO send world
        sendLine ch "."

getOrders :: forall game. Game game => game -> V.Vector Player
    -> IO (Either Faults (V.Vector (PlayerId, V.Vector (GameOrder game))))
getOrders game bots = do
    unparsedOrdersAndFaults <- forM bots $ \(Player {playerId = me, playerOutput = ch}) ->
        do
            faultOrTexts <- chReadLinesUntilDot ch
            case faultOrTexts of
                Left fault -> return (me, Left (pure fault))
                Right texts -> return (me, Right texts)
    let ioFaults :: V.Vector (PlayerId, NonEmpty Fault)
        ioFaults =
            vectorMapMaybe
                (\case
                    (botIdent, Left f) -> Just (botIdent, f)
                    _ -> Nothing)
                unparsedOrdersAndFaults
        unparsedOrders :: V.Vector (PlayerId, V.Vector T.Text)
        unparsedOrders =
            vectorMapMaybe
                (\case
                    (botIdent, Right ts) -> Just (botIdent, V.fromList ts)
                    _ -> Nothing)
                unparsedOrdersAndFaults
        badOrders :: V.Vector (PlayerId, NonEmpty Fault)
        badOrders =
            vectorMapMaybe
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
                    vectorMapMaybe
                        (\case
                            (gameParseOrder game -> Just o) -> Just o
                            _ -> Nothing)
                        os))
                unparsedOrders
    if V.null ioFaults && V.null badOrders
    then return (Right goodOrders)
    else return (Left (ioFaults <> badOrders))

vectorMapMaybe :: (a -> Maybe b) -> V.Vector a -> V.Vector b
vectorMapMaybe f = V.map (fromJust . f) . V.filter (isJust . f)

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
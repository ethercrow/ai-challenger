{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module AIChallenger.Match
    ( launchBotsAndSimulateMatch
    ) where

import Control.Exception
import Control.Monad
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

import AIChallenger.Bot
import AIChallenger.Channel
import AIChallenger.Types

launchBotsAndSimulateMatch :: Game game => game -> Turn -> V.Vector Bot -> IO ()
launchBotsAndSimulateMatch game turnLimit bots = do
    bracket (launchBots bots) (mapM_ playerClose) $ \players -> do
        GameResult winners _ _ <- simulateMatch game turnLimit players
        TIO.putStrLn
            ("Winners: " <> T.intercalate ", "
                (V.toList (fmap playerName (V.filter ((`elem` winners) . playerId) players))))

simulateMatch :: Game game => game -> Turn -> V.Vector Player -> IO (GameResult game)
simulateMatch game turnLimit bots =
    go mempty (gameInitialState game)
    where
    go turn state | turn > turnLimit = return (gameTimeout state)
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
sendWorld _ gameState =
    mapM_ $ \(Player { playerId = PlayerId me, playerInput = ch }) -> do
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
            [ (botIdent, f)
            | (botIdent, Left f) <- unparsedOrdersAndFaults
            ]
        unparsedOrders :: V.Vector (PlayerId, V.Vector T.Text)
        unparsedOrders =
            [ (botIdent, V.fromList ts)
            | (botIdent, Right ts) <- unparsedOrdersAndFaults
            ]
        badOrders :: V.Vector (PlayerId, NonEmpty Fault)
        badOrders =
            vectorMapMaybe
                (sequence . fmap
                    (\os -> (nonEmpty . V.toList)
                        [ Fault ("Failed to parse order " <> t)
                        | t@(gameParseOrder game -> Nothing) <- os
                        ]))
                unparsedOrders
        goodOrders :: V.Vector (PlayerId, V.Vector (GameOrder game))
        goodOrders =
            fmap
                (fmap (\os -> [o | (gameParseOrder game -> Just o) <- os]))
                unparsedOrders
    if V.null ioFaults && V.null badOrders
    then return (Right goodOrders)
    else return (Left (ioFaults <> badOrders))

vectorMapMaybe :: (a -> Maybe b) -> V.Vector a -> V.Vector b
vectorMapMaybe f = V.map (fromJust . f) . V.filter (isJust . f)

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
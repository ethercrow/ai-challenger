{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module RockPaperScissors
    ( game
    ) where

import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V

import AIChallenger.Types

data RPS = RPS

game :: RPS
game = RPS

data WinCounts = WinCounts Int Int
    deriving Show
data Order = R | P | S
    deriving Show
data Replay = Replay (V.Vector (WinCounts, Order, Order))
    deriving Show

instance Monoid Replay where
    mempty = Replay mempty
    mappend (Replay a) (Replay b) = Replay (mappend a b)

instance Monoid WinCounts where
    mempty = WinCounts 0 0
    mappend (WinCounts a1 b1) (WinCounts a2 b2) = WinCounts (a1 + a2) (b1 + b2)

instance Game RPS where
    type GameState RPS = (WinCounts, Replay)
    type GameOrder RPS = Order
    type GameReplay RPS = Replay
    gameInitialState _ = (mempty, mempty)
    gameParseOrder _ "R" = Just R 
    gameParseOrder _ "P" = Just P 
    gameParseOrder _ "S" = Just S 
    gameParseOrder _ _ = Nothing
    gameAdvance rawOrders (oldScore, oldReplay) =
        case validateOrders rawOrders of
            Left faults ->
                let winners =
                        V.filter
                            (not . (`V.elem` (fmap fst faults)))
                            (fmap PlayerId [0, 1])
                in Left (GameResult winners (Disqualification faults) oldReplay)
            Right orders@(o1, o2) ->
                let newScore = oldScore <> scoreRound orders
                in Right (newScore, oldReplay <> Replay [(newScore, o1, o2)])
    gameTimeout (WinCounts x y, replay) =
        let winners = case compare x y of
                EQ -> [PlayerId 1, PlayerId 2]
                GT -> [PlayerId 1]
                LT -> [PlayerId 2]
        in GameResult winners TurnLimit replay
    gameExtractReplay _1 (_state, replay) = replay

validateOrders :: V.Vector (PlayerId, V.Vector Order)
    -> Either Faults (Order, Order)
validateOrders rawOrders =
    if V.null faults
    then
        let [a, b] = foldMap (V.take 1 . snd) rawOrders
        in Right (a, b)
    else Left faults
    where
    faults = V.fromList (mapMaybe go (V.toList rawOrders))
    go (p, []) = Just (p, (pure (Fault "no moves")))
    go (_, [_]) = Nothing
    go (p, _) = Just (p, (pure (Fault "multiple moves")))

scoreRound :: (Order, Order) -> WinCounts
scoreRound (R, P) = WinCounts 0 1
scoreRound (R, S) = WinCounts 1 0
scoreRound (P, S) = WinCounts 0 1
scoreRound (P, R) = WinCounts 1 0
scoreRound (S, R) = WinCounts 0 1
scoreRound (S, P) = WinCounts 1 0
scoreRound _ = WinCounts 0 0
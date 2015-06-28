{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RockPaperScissors
    ( game
    ) where

import qualified Data.Map.Strict as M
import Data.Monoid

import AIChallenger.Types

data RPS = RPS

game :: RPS
game = RPS

data WinCounts = WinCounts Int Int
data Order = R | P | S
data Replay = Replay

instance Monoid WinCounts where
    mempty = WinCounts 0 0
    mappend (WinCounts a1 b1) (WinCounts a2 b2) = WinCounts (a1 + a2) (b1 + b2)

instance Game RPS where
    type GameState RPS = WinCounts
    type GameOrder RPS = Order
    type GameReplay RPS = Replay
    gameInitialState = mempty
    gameParseOrder _ "R" = Just R 
    gameParseOrder _ "P" = Just P 
    gameParseOrder _ "S" = Just S 
    gameParseOrder _ _ = Nothing
    gameAdvance rawOrders oldState =
        case validateOrders rawOrders of
            Left faults ->
                let winners =
                        filter
                            (not . (`elem` M.keys faults))
                            (fmap PlayerId [0, 1])
                in Left (GameResult winners (Disqualification faults) Replay)
            Right orders -> Right (oldState <> score orders)
    gameTimeout (WinCounts x y) =
        let winners = case compare x y of
                EQ -> [PlayerId 0, PlayerId 1]
                GT -> [PlayerId 0]
                LT -> [PlayerId 1]
        in GameResult winners TurnLimit Replay

validateOrders :: M.Map PlayerId [GameOrder RPS]
    -> Either Faults (GameOrder RPS, GameOrder RPS)
validateOrders rawOrders =
    if M.null faults
    then
        let [a, b] = foldMap (take 1) rawOrders
        in Right (a, b)
    else Left faults
    where
    faults = M.mapMaybe go rawOrders
    go [] = Just (pure (Fault "no moves"))
    go [x] = Nothing
    go _ = Just (pure (Fault "multiple moves"))

score :: (GameOrder RPS, GameOrder RPS) -> GameState RPS
score (R, P) = WinCounts 0 1
score (R, S) = WinCounts 1 0
score (P, S) = WinCounts 0 1
score (P, R) = WinCounts 1 0
score (S, R) = WinCounts 0 1
score (S, P) = WinCounts 1 0
score _ = WinCounts 0 0
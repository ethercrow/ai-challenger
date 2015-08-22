{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Grid
    ( game
    ) where

import Data.Maybe
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Vector.Extended as V
import Path
import System.IO
import Text.Read

import AIChallenger.Types

data GridGame = GridGame

game :: GridGame
game = GridGame

gridSize :: Int
gridSize = 10

data Score = Score Int Int

gridScore :: Grid -> Score
gridScore (Grid tiles) = Score s1 s2
    where (Sum s1, Sum s2) =
            foldMap
                (foldMap (\case
                    Empty -> (mempty, mempty)
                    Captured (PlayerId 1) -> (Sum 1, mempty)
                    Captured (PlayerId 2) -> (mempty, Sum 1)))
                tiles

data Tile
    = Empty
    | Captured PlayerId
    deriving (Show, Eq)

type Point = (Int, Int)

data Grid = Grid (V.Vector (V.Vector Tile))
    deriving (Show, Eq)
data Order = Capture Point
    deriving (Show, Eq)
data Replay = Replay (V.Vector (Grid, V.Vector Order, V.Vector Order))
    deriving (Show, Eq)

instance Monoid Replay where
    mappend (Replay r1) (Replay r2) = Replay (mappend r1 r2)

initialGrid :: Grid
initialGrid = Grid $ mconcat
        [ pure (pure (Captured (PlayerId 1)) <> V.replicate (gridSize - 1) Empty)
        , V.replicate (gridSize - 2) (V.replicate gridSize Empty)
        , pure (V.replicate (gridSize - 1) Empty <> pure (Captured (PlayerId 2)))
        ]

instance Game GridGame where
    type GameState GridGame = (Grid, Replay)
    type GameOrder GridGame = Order
    type GameReplay GridGame = Replay
    gameInitialState _ = (initialGrid, Replay (pure (initialGrid, mempty, mempty)))
    gameParseOrder _ s = case T.words s of
        ["C", tx, ty] -> do
            x <- readMaybe (T.unpack tx)
            y <- readMaybe (T.unpack ty)
            return (Capture (x, y))
        _ -> Nothing
    gameAdvance rawOrders (oldGrid, oldReplay) =
        case validateOrders rawOrders of
            Left faults ->
                let winners =
                        V.filter
                            (not . (`V.elem` (fmap fst faults)))
                            (pure (PlayerId 1) <> pure (PlayerId 2))
                in Left (GameResult winners (Disqualification faults) oldReplay)
            Right orders@(os1, os2) ->
                let newGrid = oldGrid
                in Right (newGrid, oldReplay <> Replay (pure (newGrid, os1, os2)))
    gameExtractReplay _ (_state, replay) = replay
    gameSaveReplay _ path (Replay turns) = do
        let filepath = toFilePath path
            formatTurn (grid, os1, os2) = "todo"
        withFile filepath WriteMode $ \h -> 
            mapM_ (TLIO.hPutStrLn h . formatTurn) turns
    gameSendWorld _ _state _sendLine = return ()
    gameTimeout (grid, replay) =
        let Score s1 s2 = gridScore grid
            winners = case compare s1 s2 of
                EQ -> pure (PlayerId 1) <> pure (PlayerId 2)
                GT -> pure (PlayerId 1)
                LT -> pure (PlayerId 2)
        in GameResult winners TurnLimit replay

validateOrders :: V.Vector (PlayerId, V.Vector Order)
    -> Either Faults (V.Vector Order, V.Vector Order)
validateOrders rawOrders =
    if V.null faults
    then
        let [a, b] = V.toList (fmap snd rawOrders)
        in Right (a, b)
    else Left faults
    where
    faults = V.mapMaybe go rawOrders
    go :: (PlayerId, V.Vector Order) -> Maybe (PlayerId, NE.NonEmpty Fault)
    go (p, os) =  case fmap validateOrder os of
        (V.all isNothing -> True) -> Nothing
        xs -> Just (p, foldr1 (S.<>) (V.catMaybes xs))
    validateOrder :: Order -> Maybe (NE.NonEmpty Fault)
    validateOrder (Capture (x, y)) =
        let check i | i >= gridSize || i < 0 =
                Just (pure (Fault ("Coord " <> showT i <> " is out of bounds.")))
            check _ = Nothing
        in check x S.<> check y
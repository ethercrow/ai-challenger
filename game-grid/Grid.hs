{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Grid
    ( game
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Vector.Extended as V
import qualified Data.Vector.Mutable as VM
import Path
import System.IO
import Text.Read (readMaybe)

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
    mempty = Replay mempty

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
        case validateOrders rawOrders oldGrid of
            Left faults ->
                let winners =
                        V.filter
                            (not . (`V.elem` (fmap fst faults)))
                            (pure (PlayerId 1) <> pure (PlayerId 2))
                in Left (GameResult winners (Disqualification faults) oldReplay)
            Right (os1, os2) ->
                let newGrid = oldGrid
                        & suffocate
                        & applyOrders (os1, os2)
                in Right (newGrid, oldReplay <> Replay (pure (newGrid, os1, os2)))
    gameExtractReplay _ (_state, replay) = replay
    gameSaveReplay _ path (Replay turns) = do
        let filepath = toFilePath path
            formatTurn (grid, os1, os2) = TL.intercalate "\n"
                [ "W"
                , formatGrid grid
                , "O 1"
                , formatOrders os1
                , "O 2"
                , formatOrders os2
                , "."
                ]
            formatOrders = TL.intercalate "\n"
                . fmap (\(Capture (x, y)) -> TL.pack (unwords ["C", show x, show y]))
                . V.toList
        withFile filepath WriteMode $ \h -> 
            mapM_ (TLIO.hPutStrLn h . formatTurn) turns
    gameSendWorld _ pid (grid, _) sendLn = do
        let Energy e = energyBudget grid pid
        _ <- sendLn ("E " <> showT e)
        _ <- sendLn (TL.toStrict (formatGrid grid))
        return ()
    gameTimeout (grid, replay) =
        let Score s1 s2 = gridScore grid
            winners = case compare s1 s2 of
                EQ -> pure (PlayerId 1) <> pure (PlayerId 2)
                GT -> pure (PlayerId 1)
                LT -> pure (PlayerId 2)
        in GameResult winners TurnLimit replay

formatGrid :: Grid -> TL.Text
formatGrid (Grid grid) =
    let charGrid = fmap (fmap tileToChar) grid
        tileToChar Empty = '.'
        tileToChar (Captured (PlayerId 1)) = 'X'
        tileToChar (Captured (PlayerId 2)) = 'O'
        tileToChar (Captured (PlayerId _)) = '?'
    in TL.intercalate "\n" (V.toList (fmap (TL.pack . V.toList) charGrid))

validateOrders :: V.Vector (PlayerId, V.Vector Order) -> Grid
    -> Either Faults (V.Vector Order, V.Vector Order)
validateOrders rawOrders grid =
    if V.null faults
    then
        let [a, b] = V.toList (fmap snd rawOrders)
        in Right (a, b)
    else Left faults
    where
    faults = V.mapMaybe go rawOrders
    go :: (PlayerId, V.Vector Order) -> Maybe (PlayerId, NE.NonEmpty Fault)
    go (p, os) = case fmap (validateOrder p) os of
        (V.all isNothing -> True) ->
            let Energy e = energyBudget grid p
            in if V.length os <= e
                then Just (p, pure (Fault (showT e <> " energy is not enough for " <> showT (V.length os) <> " orders")))
                else Nothing
        xs -> Just (p, foldr1 (S.<>) (V.catMaybes xs))
    validateOrder :: PlayerId -> Order -> Maybe (NE.NonEmpty Fault)
    validateOrder pid (Capture (x, y)) =
        let checkBound i | i >= gridSize || i < 0 =
                Just (pure (Fault ("Coord " <> showT i <> " is out of bounds.")))
            checkBound _ = Nothing
            checkReachability =
                if Captured pid `elem` neighborhood4 (x, y) grid
                then Nothing
                else Just (pure (Fault ("Tile " <> showT (x, y) <> " is not reachable for " <> showT pid)))
        in foldr1 (S.<>)
            [ checkBound x
            , checkBound y
            , checkReachability
            ]

suffocate :: Grid -> Grid
suffocate grid@(Grid g) =
    let surroundedPoints =
            [ (x, y)
            | x <- [0 .. gridSize - 1]
            , y <- [0 .. gridSize - 1]
            , null [() | Empty <- neighborhood8 (x, y) grid]
            ]
        killSurrounded :: VM.MVector s (V.Vector Tile) -> ST s ()
        killSurrounded mgrid =
            forM_ surroundedPoints $ \(x, y) -> do
                row <- VM.read mgrid y
                VM.write mgrid y (row V.// pure (x, Empty))
    in Grid $ V.modify killSurrounded g

neighborhood4 :: Point -> Grid -> [Tile]
neighborhood4 (x, y) (Grid grid0) =
    let neighborPoints =
            [ (x + dx, y + dy)
            | dx <- [-1, 0, 1]
            , dy <- [-1, 0, 1]
            , (dx + dy) `elem` [-1, 1]
            ]
        get (i, j) = (grid0 V.!? j) >>= (V.!? i)
    in mapMaybe get neighborPoints

neighborhood8 :: Point -> Grid -> [Tile]
neighborhood8 (x, y) (Grid grid0) =
    let neighborPoints =
            [ (x + dx, y + dy)
            | dx <- [-1, 0, 1]
            , dy <- [-1, 0, 1]
            , (dx, dy) /= (0, 0)
            ]
        get (i, j) = (grid0 V.!? j) >>= (V.!? i)
    in mapMaybe get neighborPoints

(!?) :: Grid -> Point -> Maybe Tile
(Grid grid) !? (x, y) = (grid V.!? y) >>= (V.!? x)

(!) :: Grid -> Point -> Tile
(Grid grid) ! (x, y) = (grid V.! y) V.! x

applyOrders :: (V.Vector Order, V.Vector Order) -> Grid -> Grid
applyOrders (os1, os2) (Grid grid0) = Grid $ V.modify go grid0
    where
    go :: VM.MVector s (V.Vector Tile) -> ST s ()
    go mgrid = do
        V.forM_ os1 $ \(Capture (x, y)) -> do
            row <- VM.read mgrid y
            VM.write mgrid y (row V.// pure (x, Captured (PlayerId 1)))
        V.forM_ os2 $ \(Capture (x, y)) -> do
            row <- VM.read mgrid y
            VM.write mgrid y (row V.// pure (x, Captured (PlayerId 2)))

newtype Energy = Energy Int

energyBudget :: Grid -> PlayerId -> Energy
energyBudget grid pid = Energy $ sum
    [ 1
    | x <- [0 .. gridSize - 1]
    , y <- [0 .. gridSize - 1]
    , grid ! (x, y) == Empty
    , let ns = neighborhood4 (x, y) grid
    , let friends = filter (== Captured pid) ns
    , let foes = filter (`notElem` [Empty, Captured pid]) ns
    , length friends > length foes
    ]
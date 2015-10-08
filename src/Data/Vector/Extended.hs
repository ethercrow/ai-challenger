module Data.Vector.Extended
    ( mapMaybe
    , catMaybes
    , nub
    , module V
    ) where

import Data.Maybe (fromJust, isJust)
import Data.Vector as V
import qualified Data.Set as S

mapMaybe :: (a -> Maybe b) -> V.Vector a -> V.Vector b
mapMaybe f = V.map (fromJust . f) . V.filter (isJust . f)

catMaybes :: V.Vector (Maybe a) -> V.Vector a
catMaybes = mapMaybe id

nub :: Ord a => V.Vector a -> V.Vector a
nub = V.fromList . S.toList . S.fromList . V.toList
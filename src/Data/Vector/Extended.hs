module Data.Vector.Extended
    ( mapMaybe
    , catMaybes
    , module V
    ) where

import Data.Maybe (fromJust, isJust)
import Data.Vector as V

mapMaybe :: (a -> Maybe b) -> V.Vector a -> V.Vector b
mapMaybe f = V.map (fromJust . f) . V.filter (isJust . f)

catMaybes :: V.Vector (Maybe a) -> V.Vector a
catMaybes = mapMaybe id
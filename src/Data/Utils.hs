module Data.Utils where

import Data.HashMap
import Data.Hashable
import Prelude hiding (map)

adjustOrAdd :: (Hashable k, Ord k) => (v -> v) -> v -> k -> Map k v -> Map k v
adjustOrAdd f def = alter (Just . maybe def f)

average :: (Hashable k, Fractional a) => Int -> Map k a -> Map k a
average sampleSize = map (\x -> x / (fromIntegral sampleSize))

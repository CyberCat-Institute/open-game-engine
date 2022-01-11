module OpenGames.Data.Utils where

import Prelude hiding (map)
import Data.HashMap
import Data.Hashable

adjustOrAdd :: (Hashable k, Ord k) => (v -> v) -> v -> k -> Map k v -> Map k v
adjustOrAdd f def = alter (Just . maybe def f)

average :: (Hashable k, Fractional a) => Int -> Map k a -> Map k a
average sampleSize  = map (\x -> x / (fromIntegral sampleSize))

module Data.Utils where

import Data.HashMap
import Data.Hashable

adjustOrAdd :: (Hashable k, Ord k) => (v -> v) -> v -> k -> Map k v -> Map k v
adjustOrAdd f def = alter (Just . maybe def f)


{-# LANGUAGE MultiParamTypeClasses #-}

module Engine.DecisionClass where

-- Trying to make decisions polymorphic between backends

import           Data.Profunctor

import           Engine.OpenGamesClass

class (Profunctor p, OG g) => Decision p g where
  decision :: (Eq x, Show x, Ord y, Show y) => String -> [y] -> g (p x y) x () y Double
-- the class constraints on decision are very ad-hoc,
-- coming from the needs of specific instances

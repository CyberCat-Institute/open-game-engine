{-# LANGUAGE MultiParamTypeClasses #-}

module OpenGames.Engine.DecisionClass where

-- Trying to make decisions polymorphic between backends

import           Data.Profunctor

import           OpenGames.Engine.OpenGamesClass

class (Profunctor p, OG g) => Decision p g where
  decision :: (Eq x, Show x, Ord y, Show y) => String -> [y] -> g (p x y) x () y Double
-- the class constraints on decision are very ad-hoc,
-- coming from the needs of specific instances

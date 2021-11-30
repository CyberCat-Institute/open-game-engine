{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.IOGames
  ( IOOpenGame(..)
  , Agent(..)
  , dependentDecisionIO
  , fromLens
  , fromFunctions
  ) where


import           Control.Arrow                      hiding ((+:+))
import           Control.Monad.Bayes.Weighted
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import GHC.TypeLits

import Data.Foldable
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)


import Data.List (maximumBy)
import Data.Ord (comparing)
import           Data.Utils
import Data.Vector.Generic                         as G
import System.Random.MWC.CondensedTable
import System.Random

import Engine.OpenGames hiding (lift)
import Engine.OpticClass
import Engine.TLL
import Engine.Diagnostics

---------------------------------------------
-- Reimplements stateful bayesian from before

type IOOpenGame a b x s y r = OpenGame (MonadOptic IO) (MonadContext IO) a b x s y r

type Agent = String

-- NOTE we keep the _evaluate_ part undefined as we do not need it.
dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String ->  IOOpenGame '[CondensedTableV Double] '[] x () y Double
dependentDecisionIO name = OpenGame {
  play = \(a ::- Nil) -> let v x = do pure ((),undefined)
                             in MonadOptic v (\_ -> (\_ -> pure ())),
  evaluate = undefined }


-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> IOOpenGame '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: (x -> y) -> (r -> s) -> IOOpenGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)

{-
transformProb :: Weighted m (a,b) -> a -> Weighted m b
transformProb tab x= do
  tab' <- runWeighted tab
  let reduced = Prelude.filter ((==x) . fst . fst) tab'
  withWeight reduced


-----------------------------
-- Prob support functionality

-- sample from a distribution: genFromTable

-- update a condensed table by setting values to zero.

updateTable :: Eq a => CondensedTableV (a,b) -> a -> CondensedTableV b
updateTable (CondensedTable na aa nb bb nc cc dd) = let
  tbl = G.filter ((== x) . fst) dd
  in undefined -- tbl

normalize :: CondensedTableV b -> CondensedTableV b
normalize table
      | G.null table = error "empty table"
      | otherwise = G.map (second (/ s)) table
      where
        s = G.foldl' (flip $ (+) . snd) 0 table
--}

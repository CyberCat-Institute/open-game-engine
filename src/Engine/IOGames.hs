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
  , discount
  ) where


import           Control.Arrow                      hiding ((+:+))
import           Control.Monad.Bayes.Weighted
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import           Data.Foldable
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import           Data.Utils
import qualified Data.Vector as V
import           GHC.TypeLits
import           Numeric.Probability.Distribution hiding (map, lift)
import           System.Random.MWC.CondensedTable
import           System.Random
import           System.Random.Stateful

import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           Engine.Diagnostics

-- Build game where one element is drawn
-- Ignore the Bayesian component for now
-- Build a sampler on top

---------------------------------------------
-- Reimplements stateful bayesian from before

type IOOpenGame a b x s y r = OpenGame MonadOptic MonadContext a b x s y r

type Agent = String

-- NOTE we keep the _evaluate_ part undefined as we do not need it; only play is needed
-- NOTE This ignores the state
dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String ->  IOOpenGame '[Kleisli CondensedTableV x y] '[] x () y Double
dependentDecisionIO name = OpenGame {
  play = \(a ::- Nil) -> let v x = do
                                   g <- newStdGen
                                   gS <- newIOGenM g
                                   action <- genFromTable (runKleisli a x) gS
                                   return ((),action)
                             u () r = modify (adjustOrAdd (+ r) r name)
                             in MonadOptic v u,
  evaluate = undefined }


-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> IOOpenGame '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: (x -> y) -> (r -> s) -> IOOpenGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)



-- discount Operation for repeated structures
discount :: String -> (Double -> Double) -> IOOpenGame '[] '[] () () () ()
discount name f = OpenGame {
  play = \_ -> let v () = return ((), ())
                   u () () = modify (adjustOrAdd f (f 0) name)
                 in MonadOptic v u,
  evaluate = \_ _ -> Nil}

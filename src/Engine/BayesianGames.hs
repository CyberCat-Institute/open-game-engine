{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Engine.BayesianGames
  ( StochasticStatefulBayesianOpenGame(..)
  , Agent(..)
  , dependentDecision
  , dependentEpsilonDecision
  , fromLens
  , fromFunctions
  , nature
  , liftStochastic
  , uniformDist
  , distFromList
  , pureAction
  , playDeterministically
  , discount
  ) where


import           Control.Arrow                      hiding ((+:+))
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import GHC.TypeLits

import Data.Foldable
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)


import Data.List (maximumBy)
import Data.Ord (comparing)
import           Data.Utils
import Numeric.Probability.Distribution hiding (map, lift, filter)

import Engine.OpenGames hiding (lift)
import Engine.OpticClass
import Engine.TLL
import Engine.Diagnostics

---------------------------------------------
-- Reimplements stateful bayesian from before

type StochasticStatefulBayesianOpenGame a b x s y r = OpenGame StochasticStatefulOptic StochasticStatefulContext a b x s y r

type Agent = String

support :: Stochastic x -> [x]
support = map fst . decons

bayes :: (Eq y) => Stochastic (x, y) -> y -> Stochastic x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a


deviationsInContext :: (Show x, Show y, Ord y, Show theta)
                    => Double -> Agent -> x -> theta -> Stochastic y -> (y -> Double) -> [y] -> [DiagnosticInfoBayesian x y]
deviationsInContext epsilon name x theta strategy u ys
  = [DiagnosticInfoBayesian { equilibrium = strategicPayoff >= optimalPayoff - epsilon,
                      player = name,
                      payoff = strategicPayoff,
                      optimalMove = optimalPlay,
                      optimalPayoff = optimalPayoff,
                      context = u ,
                      state = x,
                      unobservedState = show theta,
                      strategy = strategy}]
  where strategicPayoff = expected (fmap u strategy)
        (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]


dependentDecision :: (Eq x, Show x, Ord y, Show y) => String -> (x -> [y]) -> StochasticStatefulBayesianOpenGame '[Kleisli Stochastic x y] '[[DiagnosticInfoBayesian x y]] x () y Double
dependentDecision name ys = OpenGame {
  play = \(a ::- Nil) -> let v x = do {y <- runKleisli a x; return ((), y)}
                             u () r = modify (adjustOrAdd (+ r) r name)
                            in StochasticStatefulOptic v u,
  evaluate = \(a ::- Nil) (StochasticStatefulContext h k) ->
     (concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                   r <- k t y;
                                                   gets ((+ r) . HM.findWithDefault 0.0 name)})
                                    HM.empty)
                   strategy = runKleisli a x
                  in deviationsInContext 0 name x theta strategy u (ys x)
              | (theta, x) <- support h]) ::- Nil }

dependentEpsilonDecision :: (Eq x, Show x, Ord y, Show y) => Double -> String -> (x -> [y])  -> StochasticStatefulBayesianOpenGame '[Kleisli Stochastic x y] '[[DiagnosticInfoBayesian x y]] x () y Double
dependentEpsilonDecision epsilon name ys = OpenGame {
  play = \(a ::- Nil) -> let v x = do {y <- runKleisli a x; return ((), y)}
                             u () r = modify (adjustOrAdd (+ r) r name)
                            in StochasticStatefulOptic v u,
  evaluate = \(a ::- Nil) (StochasticStatefulContext h k) ->
     (concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                   r <- k t y; 
                                                   gets ((+ r) . HM.findWithDefault 0.0 name)})
                                    HM.empty)
                   strategy = runKleisli a x
                  in deviationsInContext epsilon name x theta strategy u (ys x)
              | (theta, x) <- support h]) ::- Nil }



-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> StochasticStatefulBayesianOpenGame '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> StochasticStatefulOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: (x -> y) -> (r -> s) -> StochasticStatefulBayesianOpenGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)

nature :: Stochastic x -> StochasticStatefulBayesianOpenGame '[] '[] () () x ()
nature a = OpenGame {
  play = \Nil -> StochasticStatefulOptic (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  evaluate = \Nil _ -> Nil}

liftStochastic :: (x -> Stochastic y) -> StochasticStatefulBayesianOpenGame '[] '[] x () y ()
liftStochastic f = OpenGame {
  play = \Nil -> StochasticStatefulOptic (\x -> do {y <- f x; return ((), y)}) (\() () -> return ()),
  evaluate = \_ _ -> Nil}

-- Support functionality for stochastic processes (also interface to the probability module in use)

-- uniform distribution
uniformDist = uniform

-- tailored distribution from a list
distFromList = fromFreqs

-- pure action (no randomization)
pureAction x = Kleisli $ const $ certainly x

playDeterministically :: a -> Stochastic a
playDeterministically = certainly


-- discount Operation for repeated structures
discount :: String -> (Double -> Double) -> StochasticStatefulBayesianOpenGame '[] '[] () () () ()
discount name f = OpenGame {
  play = \_ -> let v () = return ((), ())
                   u () () = modify (adjustOrAdd f (f 0) name)
                 in StochasticStatefulOptic v u,
  evaluate = \_ _ -> Nil}

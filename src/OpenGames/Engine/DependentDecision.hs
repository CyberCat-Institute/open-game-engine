{-# LANGUAGE FlexibleContexts #-}

{-
This is a patch on top of OpenGames.Engine.StatefulBayesian
Intended usage:
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
-}

module OpenGames.Engine.DependentDecision where

import Control.Arrow
import OpenGames.Engine.OpticClass
import OpenGames.Engine.Diagnostics
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import           Data.List                          (maximumBy)
import           Data.Ord                           (comparing)
import           Data.HashMap
import           Data.Utils
import           OpenGames.Engine.BayesianOpenGames (bayes, support)
import           Numeric.Probability.Distribution   hiding (lift)
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)

type Agent = String

deviationsInContext :: (Show x, Show y, Ord y, Show theta)
                    => Double -> Agent -> x -> theta -> Stochastic y -> (y -> Double) -> [y] -> [DiagnosticInfo]
deviationsInContext epsilon name x theta strategy u ys
  = if strategicPayoff >= optimalPayoff - epsilon
    then []
    else [DiagnosticInfo {player = name,
                          state = show x,
                          unobservableState = show theta,
                          strategy = show strategy,
                          payoff = show strategicPayoff,
                          optimalMove = show optimalPlay,
                          optimalPayoff = show optimalPayoff}]
  where strategicPayoff = expected (fmap u strategy)
        (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]

roleDecision :: (Eq x, Show x, Ord y, Show y) => [y] -> StochasticStatefulOpenGame (Kleisli Stochastic (Agent, x) y) (Agent, x) () y Double
roleDecision ys = OpticOpenGame {
  play = \a -> let v (name, x) = do {y <- runKleisli a (name, x); return (name, y)}
                   u name r = modify (adjustOrAdd (+ r) r name)
                in StochasticStatefulOptic v u,
  equilibrium = \(StochasticStatefulContext h k) a ->
    concat [let u y = expected (evalStateT (do {t <- lift (bayes h (name, x));
                                                r <- k t y;
                                                gets ((+ r) . findWithDefault r name)
                                               })
                                           empty)
                strategy = runKleisli a (name, x)
             in deviationsInContext 0 name x theta strategy u ys
            | (theta, (name, x)) <- support h]}

dependentDecision :: (Eq x, Show x, Ord y, Show y) => Agent -> (x -> [y]) -> StochasticStatefulOpenGame (Kleisli Stochastic x y) x () y Double
dependentDecision name ys = OpticOpenGame {
  play = \a -> let v x = do {y <- runKleisli a x; return ((), y)}
                   u () r = modify (adjustOrAdd (+ r) r name)
                in StochasticStatefulOptic v u,
  equilibrium = \(StochasticStatefulContext h k) a ->
    concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                 r <- k t y;
                                                 gets ((+ r) . findWithDefault 0.0 name)
                                                })
                                            empty)
                 strategy = runKleisli a x
              in deviationsInContext 0 name x theta strategy u (ys x)
           | (theta, x) <- support h]}

dependentRoleDecision :: (Eq x, Show x, Ord y, Show y) => (x -> [y]) -> StochasticStatefulOpenGame (Kleisli Stochastic (Agent, x) y) (Agent, x) () y Double
dependentRoleDecision ys = OpticOpenGame {
  play = \a -> let v (name, x) = do {y <- runKleisli a (name, x); return (name, y)}
                   u name r = modify (adjustOrAdd (+ r) r name)
                in StochasticStatefulOptic v u,
  equilibrium = \(StochasticStatefulContext h k) a ->
    concat [let u y = expected (evalStateT (do {t <- lift (bayes h (name, x));
                                                r <- k t y;
                                                gets ((+ r) . findWithDefault 0.0 name)
                                               })
                                           empty)
                strategy = runKleisli a (name, x)
             in deviationsInContext 0 name x theta strategy u (ys x)
            | (theta, (name, x)) <- support h]}

epsilonDecision :: (Eq x, Show x, Ord y, Show y) => Double -> Agent -> [y] -> StochasticStatefulOpenGame (Kleisli Stochastic x y) x () y Double
epsilonDecision epsilon name ys = OpticOpenGame {
  play = \a -> let v x = do {y <- runKleisli a x; return ((), y)}
                   u () r = modify (adjustOrAdd (+ r) r name)
                in StochasticStatefulOptic v u,
  equilibrium = \(StochasticStatefulContext h k) a ->
    concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                 r <- k t y;
                                                 gets ((+ r) . findWithDefault 0.0 name)
                                                })
                                            empty)
                 strategy = runKleisli a x
              in deviationsInContext epsilon name x theta strategy u ys
           | (theta, x) <- support h]}

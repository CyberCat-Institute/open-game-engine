{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenGames.Engine.BayesianGames
  ( StochasticStatefulBayesianOpenGame(..)
  , Agent(..)
  , Payoff(..)
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
  , addPayoffs
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

import OpenGames.Engine.OpenGames hiding (lift)
import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL
import OpenGames.Engine.Diagnostics
import OpenGames.Engine.Nat
import OpenGames.Engine.Vec hiding (map)

---------------------------------------------
-- Reimplements stateful bayesian from before

type StochasticStatefulBayesianOpenGame a b x s y r = OpenGame StochasticStatefulOptic StochasticStatefulContext a b x s y r

type Agent = String

type Payoff = Double

support :: Stochastic x -> [x]
support = map fst . decons

bayes :: (Eq y) => Stochastic (x, y) -> y -> Stochastic x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a


deviationsInContext :: (Show x, Show y, Ord y, Show theta)
                    => Double -> Agent -> x -> theta -> Stochastic y -> (y -> Payoff) -> [y] -> [DiagnosticInfoBayesian x y]
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


dependentDecision :: (Eq x, Show x, Ord y, Show y) => String -> (x -> [y]) -> StochasticStatefulBayesianOpenGame ('S 'Z) (Kleisli Stochastic x y) [DiagnosticInfoBayesian x y] x () y Payoff
dependentDecision name ys = OpenGame {
  play = \a -> let v x = do {y <- runKleisli a x; return ((), y)}
                   u () r = modify (adjustOrAdd (+ r) r name)
                in StochasticStatefulOptic v u,
  evaluate = \a (StochasticStatefulContext h k) ->
     (concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                   r <- k t y;
                                                   gets ((+ r) . HM.findWithDefault 0.0 name)})
                                    HM.empty)
                   strategy = runKleisli a x
                  in deviationsInContext 0 name x theta strategy u (ys x)
              | (theta, x) <- support h])
}

dependentEpsilonDecision :: (Eq x, Show x, Ord y, Show y) => Double -> String -> (x -> [y])  -> StochasticStatefulBayesianOpenGame ('S 'Z) (Kleisli Stochastic x y) [DiagnosticInfoBayesian x y] x () y Payoff
dependentEpsilonDecision epsilon name ys = OpenGame {
  play = \a -> let v x = do {y <- runKleisli a x; return ((), y)}
                   u () r = modify (adjustOrAdd (+ r) r name)
                in StochasticStatefulOptic v u,
  evaluate = \a (StochasticStatefulContext h k) ->
     (concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                   r <- k t y;
                                                   gets ((+ r) . HM.findWithDefault 0.0 name)})
                                    HM.empty)
                   strategy = runKleisli a x
                  in deviationsInContext epsilon name x theta strategy u (ys x)
              | (theta, x) <- support h])
}



-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> StochasticStatefulBayesianOpenGame 'Z () () x s y r
fromLens v u = OpenGame {
  play = \() -> StochasticStatefulOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \() _ -> ()
}


fromFunctions :: (x -> y) -> (r -> s) -> StochasticStatefulBayesianOpenGame 'Z () () x s y r
fromFunctions f g = fromLens f (const g)

nature :: Stochastic x -> StochasticStatefulBayesianOpenGame 'Z () () () () x ()
nature a = OpenGame {
  play = \() -> StochasticStatefulOptic (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  evaluate = \() _ -> ()}

liftStochastic :: (x -> Stochastic y) -> StochasticStatefulBayesianOpenGame 'Z () () x () y ()
liftStochastic f = OpenGame {
  play = \() -> StochasticStatefulOptic (\x -> do {y <- f x; return ((), y)}) (\() () -> return ()),
  evaluate = \_ _ -> ()}

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
discount :: String -> (Payoff -> Payoff) -> StochasticStatefulBayesianOpenGame 'Z () () () () () ()
discount name f = OpenGame {
  play = \_ -> let v () = return ((), ())
                   u () () = modify (adjustOrAdd f (f 0) name)
                 in StochasticStatefulOptic v u,
  evaluate = \_ _ -> ()}

-- add payoffs
addPayoffs :: String -> StochasticStatefulBayesianOpenGame 'Z () () Payoff () () ()
addPayoffs name = OpenGame {
  play = \_ -> let v x = return (x, ())
                   u value () = modify (adjustOrAdd (\x -> x + value) value name)
                 in StochasticStatefulOptic v u,
  evaluate = \_ _ -> ()}


--------------------------------------------------------------------------------------
-- Implement a version which samples the play using the Prob library build in sampling
-- Ignore the evaluate part

dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String -> (x -> [y]) -> StochasticStatefulBayesianOpenGame ('S 'Z) (Kleisli Stochastic x y) (DiagnosticInfoBayesian x y) x () y Double
dependentDecisionIO name _ = OpenGame {
  play = \a -> let v x = do { y <- runKleisli a x; return ((), y) }
                   u () r = modify (adjustOrAdd (+ r) r name)
               in StochasticStatefulOptic v u,
  evaluate = undefined }

{-}
interleave :: forall a a' b b' x y s r n m.
               Natural n
            -> Vec n (OpenGame StochasticStatefulOptic StochasticStatefulContext m a b (x, [y]) s y r)
            -> OpenGame StochasticStatefulOptic StochasticStatefulContext
                        (Mult n m)
                        (Replicate n a) (Replicate n b) ([x], [Int]) [s] [y] [r]
interleave Zero v = undefined
interleave (Succ Zero) v = _
interleave (Succ n) v = undefined
-}

interleave :: [StochasticStatefulBayesianOpenGame a b (x, [y]) s y r]
           -> StochasticStatefulBayesianOpenGame [a] [b] ([x], [Int]) [s] [y] [r]
interleave gs = StochasticStatefulBayesianOpenGame {
  play = \as -> StochasticStatefulOptic
    (\(xs, ns) -> let fwds = permute ns $ zipWith play gs as
                  in scanish (zip () xs))
    (),
  evaluate = undefined
}

permute :: [Int] -> [a] -> [a]
permute = undefined

scanish :: [(x -> [y] -> y, x)] -> [y]
scanish [] = []
scanish ((f, x) : fxs) = let ys = foo fxs
                          in f x ys : ys

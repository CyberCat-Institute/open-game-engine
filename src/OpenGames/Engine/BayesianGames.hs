{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE PartialTypeSignatures #-} -- temporary for testing

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
  , (+++)
  , g1, g2, es -- temporary for testing
  ) where


import           Control.Arrow                      hiding ((+:+), (+++))
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import GHC.TypeLits

import Data.Foldable
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)

-- temporary lol
import Unsafe.Coerce

import Data.List (maximumBy)
import Data.Ord (comparing)
import           Data.Utils
import Numeric.Probability.Distribution hiding (map, lift, filter)

import OpenGames.Engine.OpenGames hiding (lift)
import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL
import OpenGames.Engine.Diagnostics

---------------------------------------------
-- Reimplements stateful bayesian from before

type StochasticStatefulBayesianOpenGame a b x s y r =
  OpenGame StochasticStatefulOptic StochasticStatefulContext a b x s y r

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


dependentDecision :: (Eq x, Show x, Ord y, Show y) => String -> (x -> [y]) -> StochasticStatefulBayesianOpenGame '[Kleisli Stochastic x y] '[[DiagnosticInfoBayesian x y]] x () y Payoff
dependentDecision name ys = OpenGame {
  play = \(a :- Nil) -> let v x = do {y <- runKleisli a x; return ((), y)}
                            u () r = modify (adjustOrAdd (+ r) r name)
                        in StochasticStatefulOptic v u,
  evaluate = \(a :- Nil) (StochasticStatefulContext h k) ->
     (concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                   r <- k t y;
                                                   gets ((+ r) . HM.findWithDefault 0.0 name)})
                                    HM.empty)
                   strategy = runKleisli a x
                  in deviationsInContext 0 name x theta strategy u (ys x)
              | (theta, x) <- support h]) :- Nil }

dependentEpsilonDecision :: (Eq x, Show x, Ord y, Show y) => Double -> String -> (x -> [y])  -> StochasticStatefulBayesianOpenGame '[Kleisli Stochastic x y] '[[DiagnosticInfoBayesian x y]] x () y Payoff
dependentEpsilonDecision epsilon name ys = OpenGame {
  play = \(a :- Nil) -> let v x = do {y <- runKleisli a x; return ((), y)}
                            u () r = modify (adjustOrAdd (+ r) r name)
                        in StochasticStatefulOptic v u,
  evaluate = \(a :- Nil) (StochasticStatefulContext h k) ->
     (concat [ let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                   r <- k t y;
                                                   gets ((+ r) . HM.findWithDefault 0.0 name)})
                                    HM.empty)
                   strategy = runKleisli a x
                  in deviationsInContext epsilon name x theta strategy u (ys x)
              | (theta, x) <- support h]) :- Nil }



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
discount :: String -> (Payoff -> Payoff) -> StochasticStatefulBayesianOpenGame '[] '[] () () () ()
discount name f = OpenGame {
  play = \_ -> let v () = return ((), ())
                   u () () = modify (adjustOrAdd f (f 0) name)
                 in StochasticStatefulOptic v u,
  evaluate = \_ _ -> Nil}

-- add payoffs
addPayoffs :: String -> StochasticStatefulBayesianOpenGame '[] '[] Payoff () () ()
addPayoffs name = OpenGame {
  play = \_ -> let v x = return (x, ())
                   u value () = modify (adjustOrAdd (\x -> x + value) value name)
                 in StochasticStatefulOptic v u,
  evaluate = \_ _ -> Nil}


--------------------------------------------------------------------------------------
-- Implement a version which samples the play using the Prob library build in sampling
-- Ignore the evaluate part

dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String -> (x -> [y]) -> StochasticStatefulBayesianOpenGame '[Kleisli Stochastic x y] '[[DiagnosticInfoBayesian x y]] x () y Double
dependentDecisionIO name _ = OpenGame {
  play = \(a :- Nil) -> let v x = do
                              y <- runKleisli a x
                              return ((), y)
                            u () r = modify (adjustOrAdd (+ r) r name)
                        in StochasticStatefulOptic v u,
  evaluate = undefined }

unsafeConcat :: forall b1 b2. List (TMap Maybe b1) -> List (TMap Maybe b2) -> List (TMap Maybe (b1 +:+ b2))
unsafeConcat = unsafeCoerce (+:+)

(+++) :: forall a1 a2 b1 b2 x1 x2 s r y1 y2. (Unappend a1, Unappend a2, RepNothing b1, RepNothing b2)
      => StochasticStatefulBayesianOpenGame a1 b1 x1 s y1 r
      -> StochasticStatefulBayesianOpenGame a2 b2 x2 s y2 r
      -> StochasticStatefulBayesianOpenGame (a1 +:+ a2) (TMap Maybe (b1 +:+ b2)) (Either x1 x2) s (Either y1 y2) r
(+++) g1 g2 = OpenGame {
  play = \as -> case unappend as of (a1, a2) -> play g1 a1 ++++ play g2 a2,
  evaluate = \as (StochasticStatefulContext h k) ->
    case unappend as of
      ((a1, a2) :: (List a1, List a2)) ->
          let xs1 = [((z, x1), p) | ((z, Left x1), p) <- decons h]
              xs2 = [((z, x2), p) | ((z, Right x2), p) <- decons h]
              e1 = evaluate g1 a1 (StochasticStatefulContext (fromFreqs xs1) (\z y1 -> k z (Left y1)))
              e2 = evaluate g2 a2 (StochasticStatefulContext (fromFreqs xs2) (\z y2 -> k z (Right y2)))
              -- Warning: evil laziness trick
              -- "fromFreqs xs" throws an exception when xs is null
              -- It is possible for either xs1 or xs2 to be null, but not both
              -- (because a probability distribution on a disjoint union must
              -- be supported on at least one component)
           in case (null xs1, null xs2) of
                (False, False) -> vmap Just (e1 +:+ e2)
                (False, True)  -> unsafeConcat @b1 @b2 (vmap Just e1) (rep @b2)
                (True, False)  -> unsafeConcat @b1 @b2 (rep @b1) (vmap Just e2)
                _              -> error "This can't happen"
}


-- temporary testing for +++

data X = X1 | X2 deriving (Eq, Ord, Show)
data Y = Y1 | Y2 deriving (Eq, Ord, Show)
g1 :: StochasticStatefulBayesianOpenGame _ _ () () X Payoff
g1 = dependentDecision "player1" (const [X1,X2])
g2 :: StochasticStatefulBayesianOpenGame _ _ () () Y Payoff
g2 = dependentDecision "player2" (const [Y1,Y2])

k :: () -> Either X Y -> StateT Vector Stochastic Payoff
k () (Left X1) = return 1
k () (Left X2) = return 2
k () (Right Y1) = return 3
k () (Right Y2) = return 4

es = generateOutput $ evaluate (g1 +++ g2)
              (Kleisli (\() -> certainly X2) :- Kleisli (\() -> certainly Y1) :- Nil)
              (StochasticStatefulContext (fromFreqs [(((), Left ()), 1000), (((), Right ()), 1)]) k)

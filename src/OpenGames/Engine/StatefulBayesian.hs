{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenGames.Engine.StatefulBayesian where

-- Subsumes both BayesianDiagnostics and StatefulPayoffs

import           Control.Arrow
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import           Data.Profunctor
import           Data.HashMap                       as HM hiding (null)
import           Numeric.Probability.Distribution   hiding (lift)

import           Data.List                          (maximumBy)
import           Data.Ord                           (comparing)
import           OpenGames.Engine.BayesianOpenGames (bayes, support)
import           OpenGames.Engine.DecisionClass
import           OpenGames.Engine.Diagnostics
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.SubgamePerfect    (KleisliContext (..),
                                                     KleisliOptic (..))

type Stochastic = T Double
type Vector = HM.Map String Double

data StochasticStatefulOptic s t a b where
  StochasticStatefulOptic :: Eq z => (s -> Stochastic (z, a))
                          -> (z -> b -> StateT Vector Stochastic t)
                          -> StochasticStatefulOptic s t a b

instance (Eq s, Eq t, Eq a, Eq b) => Eq (StochasticStatefulOptic s t a b) where
  (==) (StochasticStatefulOptic f1 f2)
       (StochasticStatefulOptic g1 g2) = undefined

instance Optic StochasticStatefulOptic where
  lens v u = StochasticStatefulOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where v (Left s1)  = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b  = u1 z1 b
          u (Right z2) b = u2 z2 b

data StochasticStatefulContext s t a b where
  StochasticStatefulContext :: (Show z) => Stochastic (z, s) -> (z -> a -> StateT Vector Stochastic b) -> StochasticStatefulContext s t a b

instance Context StochasticStatefulContext StochasticStatefulOptic where
  void = StochasticStatefulContext (return ((), ())) (\() () -> return ())
  cmap (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) (StochasticStatefulContext h k)
            = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
                  k' z a = do {(z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'}
               in StochasticStatefulContext h' k'
  (//) (StochasticStatefulOptic v u) (StochasticStatefulContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
                  k' (z, s1) a2 = do {(_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2}
               in StochasticStatefulContext h' k'

instance ContextAdd StochasticStatefulContext where
  prl (StochasticStatefulContext h k)
    = let fs = [((z, s1), p) | ((z, Left s1), p) <- decons h]
       in if null fs then Nothing
                     else Just (StochasticStatefulContext (fromFreqs fs) (\z a1 -> k z (Left a1)))
  prr (StochasticStatefulContext h k)
    = let fs = [((z, s2), p) | ((z, Right s2), p) <- decons h]
       in if null fs then Nothing
                     else Just (StochasticStatefulContext (fromFreqs fs) (\z a2 -> k z (Right a2)))

type StochasticStatefulOpenGame = OpticOpenGame StochasticStatefulOptic StochasticStatefulContext [DiagnosticInfo]

instance Show (Kleisli Stochastic a b) where
  show _ = "kleisli stochastic"

instance Decision (Kleisli Stochastic) StochasticStatefulOpenGame where
  decision name ys = OpticOpenGame {
    play = \a -> let v x = do {y <- runKleisli a x; return ((), y)}
                     u () r = modify (HM.adjust (+ r) name)
                  in StochasticStatefulOptic v u,
    equilibrium = \(StochasticStatefulContext h k) a ->
      concat [let u y = expected (evalStateT (do {t <- lift (bayes h x);
                                                  r <- k t y;
                                                  gets ((+ r) . HM.findWithDefault 0.0 name)
                                                 })
                                             HM.empty)
                  strategy = runKleisli a x
                  strategicPayoff = expected (fmap u strategy)
                  (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
               in if strategicPayoff >= optimalPayoff
                     then []
                     else [DiagnosticInfo {player        = name,
                                           state         = show x,
                                           unobservableState = show theta,
                                           strategy      = show strategy,
                                           payoff        = show strategicPayoff,
                                           optimalMove   = show optimalPlay,
                                           optimalPayoff = show optimalPayoff}]
             | (theta, x) <- support h]}

{-
roleDecision and dependentDecision are wrong but are kept here for backward compatibility with older examples
Intended usage:
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
-}

roleDecision :: (Eq x, Show x, Ord y, Show y)
             => [y] -> StochasticStatefulOpenGame (Kleisli Stochastic x y) (String, x) () y Double
roleDecision ys = OpticOpenGame {
  play = \a -> let v (name, x) = do {y <- runKleisli a x; return (name, y)}
                   u name r = modify (HM.adjust (+ r) name)
                in StochasticStatefulOptic v u,
  equilibrium = \(StochasticStatefulContext h k) a ->
    concat [let u y = expected (evalStateT (do {t <- lift (bayes h (name, x));
                                                r <- k t y;
                                                gets ((+ r) . HM.findWithDefault 0.0 name)
                                               })
                                           HM.empty)
                strategy = runKleisli a x
                strategicPayoff = expected (fmap u strategy)
                (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
             in if strategicPayoff >= optimalPayoff
                   then []
                   else [DiagnosticInfo {player        = name,
                                         state         = show x,
                                         unobservableState = show theta,
                                         strategy      = show strategy,
                                         payoff        = show strategicPayoff,
                                         optimalMove   = show optimalPlay,
                                         optimalPayoff = show optimalPayoff}]
           | (theta, (name, x)) <- support h]}

dependentDecision :: (Eq x, Show x, Ord y, Show y)
                  => StochasticStatefulOpenGame (Kleisli Stochastic x y) (String, [y], x) () y Double
dependentDecision = OpticOpenGame {
  play = \a -> let v (name, _, x) = do {y <- runKleisli a x; return (name, y)}
                   u name r = modify (HM.adjust (+ r) name)
                in StochasticStatefulOptic v u,
  equilibrium = \(StochasticStatefulContext h k) a ->
    concat [let u y = expected (evalStateT (do {t <- lift (bayes h (name, ys, x));
                                                r <- k t y;
                                                gets ((+ r) . HM.findWithDefault 0.0 name)
                                               })
                                           HM.empty)
                strategy = runKleisli a x
                strategicPayoff = expected (fmap u strategy)
                (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
             in if strategicPayoff >= optimalPayoff
                   then []
                   else [DiagnosticInfo {player        = name,
                                         state         = show x,
                                         unobservableState = show theta,
                                         strategy      = show strategy,
                                         payoff        = show strategicPayoff,
                                         optimalMove   = show optimalPlay,
                                         optimalPayoff = show optimalPayoff}]
           | (theta, (name, ys, x)) <- support h]}

nature :: Eq x => Stochastic x -> StochasticStatefulOpenGame () () () x ()
nature a = OpticOpenGame {
  play = \() -> StochasticStatefulOptic (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
 equilibrium = \_ () -> []}

liftStochastic :: (Eq x, Eq y) => (x -> Stochastic y) -> StochasticStatefulOpenGame () x () y ()
liftStochastic f = OpticOpenGame {
  play = \() -> let v x = do {y <- f x; return ((), y)}
                    u () _ = return ()
                 in StochasticStatefulOptic v u,
  equilibrium = \_ () -> []}

discount :: String -> (Double -> Double) -> StochasticStatefulOpenGame () () () () ()
discount name f = OpticOpenGame {
  play = \() -> let v () = return ((), ())
                    u () () = modify (HM.adjust f name)
                 in StochasticStatefulOptic v u,
  equilibrium = \_ () -> []}

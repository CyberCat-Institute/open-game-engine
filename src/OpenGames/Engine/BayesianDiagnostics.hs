{-# LANGUAGE GADTs #-}

module OpenGames.Engine.BayesianDiagnostics where

-- This is a different implementation of bayesian open games that constrains the quantified type to be in Show
-- which is needed for getting more useful diagnostic information
-- The Show constraint propagates everywhere and we can't use the open games class anymore,
-- so instead we use the same names and let metaprogramming take care of it

import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianOpenGames hiding (C, trivialContext, cmap, lcancel, rcancel, play, equilibrium)

data C prob x s y r where C :: (Show a) => T prob (a, x) -> (a -> y -> T prob r) -> C prob x s y r

trivialContext :: (Num prob) => C prob () () () ()
trivialContext = C (return ((), ())) (\() () -> return ())

cmap :: (Num prob) => L prob x s x' s' -> L prob y' r' y r -> C prob x s y r -> C prob x' s' y' r'
cmap (L vl ul) (L vm um) (C h k) = C h' k' where
  h' = do {(a, x) <- h; (_, x') <- vl x; return (a, x')}
  k' a y' = do {(b, y) <- vm y'; r <- k a y; um b r}

lcancel :: (Show x, Num prob) => L prob x s y r -> C prob (x, x') (s, s') (y, y') (r, r') -> C prob x' s' y' r'
lcancel (L v u) (C h k) = C h' k' where
  h' = do {(a, (x, x')) <- h; return ((a, x), x')}
  k' (a, x) y' = do {(_, y) <- v x; (_, r') <- k a (y, y'); return r'}

rcancel :: (Show x', Num prob) => L prob x' s' y' r' -> C prob (x, x') (s, s') (y, y') (r, r') -> C prob x s y r
rcancel (L v u) (C h k) = C h' k' where
  h' = do {(a, (x, x')) <- h; return ((a, x'), x)}
  k' (a, x') y = do {(_, y') <- v x'; (r, _) <- k a (y, y'); return r}

-- We also change the diagnostic tyoe because we can say more about a Bayesian game

data DiagnosticInfo = DiagnosticInfo {
  player          :: String,
  observedState   :: String,
  unobservedState :: String,
  strategy        :: String,
  payoff          :: String,
  optimalMove     :: String,
  optimalPayoff   :: String}
  deriving (Show)

data BayesianDiagnosticOpenGame prob a x s y r = BayesianDiagnosticOpenGame {
  play :: a -> L prob x s y r,
  equilibrium :: C prob x s y r -> a -> [DiagnosticInfo]}

fromLens :: (Num prob) => (x -> y) -> (x -> r -> s) -> BayesianDiagnosticOpenGame prob () x s y r
fromLens v u = BayesianDiagnosticOpenGame {
  play = \() -> L (\x -> return (x, v x)) (\x r -> return (u x r)),
  equilibrium = \_ () -> []}

reindex :: (a -> b) -> BayesianDiagnosticOpenGame prob b x s y r -> BayesianDiagnosticOpenGame prob a x s y r
reindex f g = BayesianDiagnosticOpenGame {
  play = \a -> play g (f a),
  equilibrium = \c a -> equilibrium g c (f a)}

(>>>) :: (Num prob) => BayesianDiagnosticOpenGame prob a x s y r -> BayesianDiagnosticOpenGame prob b y r z q -> BayesianDiagnosticOpenGame prob (a, b) x s z q
(>>>) g1 g2 = BayesianDiagnosticOpenGame {
  play = \(a, b) -> play g1 a >>>> play g2 b,
  equilibrium = \c (a, b) -> equilibrium g1 (cmap (iso id id) (play g2 b) c) a ++ equilibrium g2 (cmap (play g1 a) (iso id id) c) b}

-- Nb. (&&&) is the only thing whose type genuinely changes, we had to abandon the OG class because of it

(&&&) :: (Show x1, Show x2, Num prob) => BayesianDiagnosticOpenGame prob a x1 s1 y1 r1 -> BayesianDiagnosticOpenGame prob b x2 s2 y2 r2 -> BayesianDiagnosticOpenGame prob (a, b) (x1, x2) (s1, s2) (y1, y2) (r1, r2)
(&&&) g1 g2 = BayesianDiagnosticOpenGame {
  play = \(a, b) -> play g1 a &&&& play g2 b,
  equilibrium = \c (a, b) -> equilibrium g1 (rcancel (play g2 b) c) a ++ equilibrium g2 (lcancel (play g1 a) c) b}

fromFunctions :: (Num prob) => (x -> y) -> (r -> s) -> BayesianDiagnosticOpenGame prob () x s y r
fromFunctions f g = fromLens f (const g)

counit :: (Num prob) => BayesianDiagnosticOpenGame prob () x x () ()
counit = fromLens (const ()) const

counitFunction :: (Num prob) => (x -> y) -> BayesianDiagnosticOpenGame prob () x y () ()
counitFunction f = fromLens (const ()) (const . f)

nature :: (Num prob) => T prob x -> BayesianDiagnosticOpenGame prob () () () x ()
nature a = BayesianDiagnosticOpenGame {
  play = \() -> L (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  equilibrium = \_ () -> []}

liftStochastic :: (Num prob) => (x -> T prob y) -> BayesianDiagnosticOpenGame prob () x () y ()
liftStochastic f = BayesianDiagnosticOpenGame {
  play = \() -> L (\x -> do {y <- f x; return ((), y)}) (\() () -> return ()),
  equilibrium = \_ _ -> []}

decision :: (Eq x, Show x, Ord y, Show y, Ord prob, Fractional prob, Show prob) => String -> [y] -> BayesianDiagnosticOpenGame prob (x -> T prob y) x () y prob
decision name ys = BayesianDiagnosticOpenGame {
  play = \a -> L (\x -> do {y <- a x; return ((), y)}) (\() _ -> return ()),
  equilibrium = \(C h k) a -> concat [ let u y = expected (do {t <- bayes h x; k t y})
                                           strategy                     = a x
                                           strategicPayoff              = expected (fmap u strategy)
                                           (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
                                        in if strategicPayoff >= optimalPayoff then []
                                                                               else [DiagnosticInfo {player          = name,
                                                                                                     observedState   = show x,
                                                                                                     unobservedState = show theta,
                                                                                                     strategy        = show strategy,
                                                                                                     payoff          = show strategicPayoff,
                                                                                                     optimalMove     = show optimalPlay,
                                                                                                     optimalPayoff   = show optimalPayoff}]
                                     | (theta, x) <- support h ]}

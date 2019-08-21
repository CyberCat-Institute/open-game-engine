{-# LANGUAGE GADTs #-}

module OpenGames.Engine.BayesianOpenGames where

-- Bayesian open games implemented via Riley optics
-- All numbers are rational
-- Generic over a monoid of truth-values, which can be used to return debugging information about a strategy profile

import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric.Probability.Distribution hiding (map)

import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.Diagnostics

-- Probability monad

type D = T Rational

support :: D x -> [x]
support = map fst . decons

bayes :: (Eq y) => D (x, y) -> y -> D x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a

-- Riley lenses over the kleisli category of D

data L x s y r where L :: (x -> D (a, y)) -> (a -> r -> D s) -> L x s y r

iso :: (x -> y) -> (r -> s) -> L x s y r
iso f g = L (\x -> return ((), f x)) (\() r -> return (g r))

(>>>>) :: L x s y r -> L y r z q -> L x s z q
(>>>>) (L vl ul) (L vm um) = L v u where
  v x = do {(a, y) <- vl x; (b, z) <- vm y; return ((a, b), z)}
  u (a, b) q = do {r <- um b q; ul a r}

(&&&&) :: L x s y r -> L x' s' y' r' -> L (x, x') (s, s') (y, y') (r, r')
(&&&&) (L vl ul) (L vm um) = L v u where
  v (x, x') = do {(a, y) <- vl x; (b, y') <- vm x'; return ((a, b), (y, y'))}
  u (a, b) (r, r') = do {s <- ul a r; s' <- um b r'; return (s, s')}

-- Contexts

data C x s y r where C :: D (a, x) -> (a -> y -> D r) -> C x s y r

trivialContext :: C () () () ()
trivialContext = C (return ((), ())) (\() () -> return ())

cmap :: L x s x' s' -> L y' r' y r -> C x s y r -> C x' s' y' r'
cmap (L vl ul) (L vm um) (C h k) = C h' k' where
  h' = do {(a, x) <- h; (_, x') <- vl x; return (a, x')}
  k' a y' = do {(b, y) <- vm y'; r <- k a y; um b r}

lcancel :: L x s y r -> C (x, x') (s, s') (y, y') (r, r') -> C x' s' y' r'
lcancel (L v u) (C h k) = C h' k' where
  h' = do {(a, (x, x')) <- h; return ((a, x), x')}
  k' (a, x) y' = do {(_, y) <- v x; (_, r') <- k a (y, y'); return r'}

rcancel :: L x' s' y' r' -> C (x, x') (s, s') (y, y') (r, r') -> C x s y r
rcancel (L v u) (C h k) = C h' k' where
  h' = do {(a, (x, x')) <- h; return ((a, x'), x)}
  k' (a, x') y = do {(_, y') <- v x'; (r, _) <- k a (y, y'); return r}

-- Bayesian open games

data BayesianOpenGame m a x s y r = BayesianOpenGame {
  play :: a -> L x s y r,
  equilibrium :: C x s y r -> a -> m}

instance (Monoid m) => OG (BayesianOpenGame m) where
  fromLens v u = BayesianOpenGame {
    play = \() -> L (\x -> return (x, v x)) (\x r -> return (u x r)),
    equilibrium = \_ () -> mempty}
  reindex f g = BayesianOpenGame {
    play = \a -> play g (f a),
    equilibrium = \c a -> equilibrium g c (f a)}
  (>>>) g1 g2 = BayesianOpenGame {
    play = \(a, b) -> play g1 a >>>> play g2 b,
    equilibrium = \c (a, b) -> equilibrium g1 (cmap (iso id id) (play g2 b) c) a `mappend` equilibrium g2 (cmap (play g1 a) (iso id id) c) b}
  (&&&) g1 g2 = BayesianOpenGame {
    play = \(a, b) -> play g1 a &&&& play g2 b,
    equilibrium = \c (a, b) -> equilibrium g1 (rcancel (play g2 b) c) a `mappend` equilibrium g2 (lcancel (play g1 a) c) b}

nature :: (Monoid m) => D x -> BayesianOpenGame m () () () x ()
nature a = BayesianOpenGame {
  play = \() -> L (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  equilibrium = \_ () -> mempty}

bayesianDecision :: (Eq x) => [y] -> BayesianOpenGame Bool (x -> D y) x () y Rational
bayesianDecision ys = BayesianOpenGame {
  play = \a -> L (\x -> do {y <- a x; return ((), y)}) (\() _ -> return ()),
  equilibrium = \(C h k) a -> all (\x -> let u y = expected (do {t <- bayes h x; k t y})
                                          in expected (fmap u (a x)) >= maximum [u y | y <- ys])
                                  (map snd (support h))}

bayesianDecision1 :: [y] -> BayesianOpenGame Bool (D y) () () y Rational
bayesianDecision1 = reindex const . bayesianDecision

bayesianDecisionDiagnostic :: (Eq x, Show x, Ord y, Show y) => String -> [y] -> BayesianOpenGame [DiagnosticInfo] (x -> D y) x () y Rational
bayesianDecisionDiagnostic name ys = BayesianOpenGame {
  play = \a -> L (\x -> do {y <- a x; return ((), y)}) (\() _ -> return ()),
  equilibrium = \(C h k) a -> concat [ let u y = expected (do {t <- bayes h x; k t y})
                                           strategy                     = a x
                                           strategicPayoff              = expected (fmap u strategy)
                                           (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
                                        in if strategicPayoff >= optimalPayoff then []
                                                                               else [DiagnosticInfo {player        = name,
                                                                                                     state         = show x,
                                                                                                     strategy      = show strategy,
                                                                                                     payoff        = show strategicPayoff,
                                                                                                     optimalMove   = show optimalPlay,
                                                                                                     optimalPayoff = show optimalPayoff}]
                                     | (theta, x) <- support h ]}


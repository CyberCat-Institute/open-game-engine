{-# LANGUAGE GADTs #-}

module OpenGames.Engine.BayesianOpenGames where

-- Bayesian open games implemented via Riley optics
-- All numbers are rational
-- Generic over a monoid of truth-values, which can be used to return debugging information about a strategy profile

import           Data.List                        (maximumBy)
import           Data.Ord                         (comparing)
import           Numeric.Probability.Distribution hiding (map)

import           OpenGames.Engine.Diagnostics
import           OpenGames.Engine.OpenGamesClass

-- Probability monad

type D = T Rational

support :: T prob x -> [x]
support = map fst . decons

bayes :: (Eq y, Fractional prob) => T prob (x, y) -> y -> T prob x
bayes a y = mapMaybe (\(x, y') -> if y' == y then Just x else Nothing) a

-- Riley lenses over the kleisli category of D

data L prob x s y r where L :: (x -> T prob (a, y)) -> (a -> r -> T prob s) -> L prob x s y r

iso :: (Num prob) => (x -> y) -> (r -> s) -> L prob x s y r
iso f g = L (\x -> return ((), f x)) (\() r -> return (g r))

(>>>>) :: (Num prob) => L prob x s y r -> L prob y r z q -> L prob x s z q
(>>>>) (L vl ul) (L vm um) = L v u where
  v x = do {(a, y) <- vl x; (b, z) <- vm y; return ((a, b), z)}
  u (a, b) q = do {r <- um b q; ul a r}

(&&&&) :: (Num prob) => L prob x s y r -> L prob x' s' y' r' -> L prob (x, x') (s, s') (y, y') (r, r')
(&&&&) (L vl ul) (L vm um) = L v u where
  v (x, x') = do {(a, y) <- vl x; (b, y') <- vm x'; return ((a, b), (y, y'))}
  u (a, b) (r, r') = do {s <- ul a r; s' <- um b r'; return (s, s')}

(++++) :: (Num prob) => L prob x s y r -> L prob x' s y' r -> L prob (Either x x') s (Either y y') r
(++++) (L vl ul) (L vm um) = L v u where
  v (Left x)   = do {(a, y) <- vl x; return (Left a, Left y)}
  v (Right x') = do {(a', y') <- vm x'; return (Right a', Right y')}
  u (Left a) r   = ul a r
  u (Right a') r = um a' r

(+++++) :: (Num prob) => L prob x s y r -> L prob x' s' y' r'
                      -> L prob (Either x x') (Either s s') (Either y y') (Either r r')
(+++++) (L vl ul) (L vm um) = L v u where
  v (Left x)   = do {(a, y) <- vl x; return (Left a, Left y)}
  v (Right x') = do {(a', y') <- vm x'; return (Right a', Right y')}
  u (Left a)   (Left r)   = do {s <- ul a r; return (Left s)}
  u (Right a') (Right r') = do {s' <- um a' r'; return (Right s')}
  u _          _          = error "Use Idris instead"

-- Contexts

data C prob x s y r where C :: T prob (a, x) -> (a -> y -> T prob r) -> C prob x s y r

trivialContext :: (Num prob) => C prob () () () ()
trivialContext = C (return ((), ())) (\() () -> return ())

cmap :: (Num prob) => L prob x s x' s' -> L prob y' r' y r -> C prob x s y r -> C prob x' s' y' r'
cmap (L vl ul) (L vm um) (C h k) = C h' k' where
  h' = do {(a, x) <- h; (_, x') <- vl x; return (a, x')}
  k' a y' = do {(b, y) <- vm y'; r <- k a y; um b r}

lcancel :: (Num prob) => L prob x s y r -> C prob (x, x') (s, s') (y, y') (r, r') -> C prob x' s' y' r'
lcancel (L v u) (C h k) = C h' k' where
  h' = do {(a, (x, x')) <- h; return ((a, x), x')}
  k' (a, x) y' = do {(_, y) <- v x; (_, r') <- k a (y, y'); return r'}

rcancel :: (Num prob) => L prob x' s' y' r' -> C prob (x, x') (s, s') (y, y') (r, r') -> C prob x s y r
rcancel (L v u) (C h k) = C h' k' where
  h' = do {(a, (x, x')) <- h; return ((a, x'), x)}
  k' (a, x') y = do {(_, y') <- v x'; (r, _) <- k a (y, y'); return r}

-- Bayesian open games

data BayesianOpenGame m a x s y r = BayesianOpenGame {
  play        :: a -> L Rational x s y r,
  equilibrium :: C Rational x s y r -> a -> m}

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
  (+++) g1 g2 = BayesianOpenGame {
    play = \(a, b) -> play g1 a ++++ play g2 b,
    equilibrium = \(C h k) (a, b) -> let xs1 = [((a, x), p) | ((a, Left x), p) <- decons h]
                                         xs2 = [((a, x'), p) | ((a, Right x'), p) <- decons h]
                                         e1 = equilibrium g1 (C (fromFreqs xs1) (\a y1 -> k a (Left y1))) a
                                         e2 = equilibrium g2 (C (fromFreqs xs2) (\a y2 -> k a (Right y2))) b
                                      in if null xs2 then e1
                                         else if null xs1 then e2
                                         else e1 `mappend` e2}

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

bayesianDecision1 :: Eq y => [y] -> BayesianOpenGame Bool (D y) () () y Rational
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
                                                                                                     unobservableState = "",
                                                                                                     strategy      = show strategy,
                                                                                                     payoff        = show strategicPayoff,
                                                                                                     optimalMove   = show optimalPlay,
                                                                                                     optimalPayoff = show optimalPayoff}]
                                     | (theta, x) <- support h ]}

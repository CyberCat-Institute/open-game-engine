{-# LANGUAGE TypeOperators, DataKinds, GADTs, PolyKinds, TypeFamilies #-}

module Engine.BayesianDiagnosticsTLL where

-- Bayesian open games with extra diagnostics and type-level lists for strategy profiles

import Data.List (maximumBy)
import Data.Ord (comparing)
import Numeric.Probability.Distribution hiding (map)

import Engine.BayesianOpenGames (D, support, bayes, L (..), (>>>>), (&&&&), iso)
import Engine.BayesianDiagnostics (DiagnosticInfo (..), C (..), cmap, lcancel, rcancel)

infixr 6 :-
data List ts where
  Nil :: List '[]
  (:-) :: t -> List ts -> List (t ': ts)

type family (++) (as :: [*]) (bs :: [*]) :: [*]  where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

class Unappend as where
  unappend :: List (as ++ bs) -> (List as, List bs)
instance Unappend '[] where
  unappend bs = (Nil, bs)
instance Unappend as => Unappend (a ': as) where
  unappend (a :- abs) = case unappend abs of (as, bs) -> (a :- as, bs)
  
append :: List as -> List bs -> List (as ++ bs)
append Nil bs = bs
append (a :- as) bs = a :- append as bs

data BayesianDiagnosticOpenGameTLL as x s y r = BayesianDiagnosticOpenGameTLL {
  play :: List as -> L Rational x s y r,
  equilibrium :: C Rational x s y r -> List as -> [DiagnosticInfo]}

fromLens :: (x -> y) -> (x -> r -> s) -> BayesianDiagnosticOpenGameTLL '[] x s y r
fromLens v u = BayesianDiagnosticOpenGameTLL {
  play = \Nil -> L (\x -> return (x, v x)) (\x r -> return (u x r)),
  equilibrium = \_ Nil -> []}

(>>>) :: (Unappend a) => BayesianDiagnosticOpenGameTLL a x s y r -> BayesianDiagnosticOpenGameTLL b y r z q -> BayesianDiagnosticOpenGameTLL (a ++ b) x s z q
(>>>) g1 g2 = BayesianDiagnosticOpenGameTLL {
  play = \ab -> case unappend ab of (a, b) -> play g1 a >>>> play g2 b,
  equilibrium = \c ab -> case unappend ab of (a, b) -> equilibrium g1 (cmap (iso id id) (play g2 b) c) a ++ equilibrium g2 (cmap (play g1 a) (iso id id) c) b}

(&&&) :: (Unappend a, Show x1, Show x2) => BayesianDiagnosticOpenGameTLL a x1 s1 y1 r1
                                        -> BayesianDiagnosticOpenGameTLL b x2 s2 y2 r2
                                        -> BayesianDiagnosticOpenGameTLL (a ++ b) (x1, x2) (s1, s2) (y1, y2) (r1, r2)
(&&&) g1 g2 = BayesianDiagnosticOpenGameTLL {
  play = \ab -> case unappend ab of (a, b) -> play g1 a &&&& play g2 b,
  equilibrium = \c ab -> case unappend ab of (a, b) -> equilibrium g1 (rcancel (play g2 b) c) a ++ equilibrium g2 (lcancel (play g1 a) c) b}

fromFunctions :: (x -> y) -> (r -> s) -> BayesianDiagnosticOpenGameTLL '[] x s y r
fromFunctions f g = fromLens f (const g)

counit :: BayesianDiagnosticOpenGameTLL '[] x x () ()
counit = fromLens (const ()) const

counitFunction :: (x -> y) -> BayesianDiagnosticOpenGameTLL '[] x y () ()
counitFunction f = fromLens (const ()) (const . f)

nature :: D x -> BayesianDiagnosticOpenGameTLL '[] () () x ()
nature a = BayesianDiagnosticOpenGameTLL {
  play = \Nil -> L (\() -> do {x <- a; return ((), x)}) (\() () -> return ()),
  equilibrium = \_ Nil -> []}

decision :: (Eq x, Show x, Ord y, Show y) => String -> [y] -> BayesianDiagnosticOpenGameTLL '[x -> D y] x () y Rational
decision name ys = BayesianDiagnosticOpenGameTLL {
  play = \(a :- Nil) -> L (\x -> do {y <- a x; return ((), y)}) (\() _ -> return ()),
  equilibrium = \(C h k) (a :- Nil) -> concat [ let u y = expected (do {t <- bayes h x; k t y})
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

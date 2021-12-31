{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module OpenGames.Engine.SubgamePerfect where

-- Experimental approach to detecting subgame perfection

import OpenGames.Engine.OpticClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.Diagnostics -- for instance Monoid Bool

data KleisliOptic m s t a b where
  KleisliOptic :: (s -> m (z, a)) -> (z -> b -> m t) -> KleisliOptic m s t a b

instance (Monad m) => Optic (KleisliOptic m) where
  lens v u = KleisliOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (KleisliOptic v1 u1) (KleisliOptic v2 u2) = KleisliOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (KleisliOptic v1 u1) (KleisliOptic v2 u2) = KleisliOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (KleisliOptic v1 u1) (KleisliOptic v2 u2) = KleisliOptic v u
    where v (Left s1) = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b = u1 z1 b
          u (Right z2) b = u2 z2 b

data KleisliContext m s t a b where
  KleisliContext :: m (z, s) -> (z -> a -> m b) -> KleisliContext m s t a b

instance (Monad m) => Context (KleisliContext m) (KleisliOptic m) where
  void = KleisliContext (return ((), ())) (\() () -> return ())
  cmap (KleisliOptic v1 u1) (KleisliOptic v2 u2) (KleisliContext h k) = KleisliContext h' k'
    where h' = do {(z, s1) <- h; (_, s2) <- v1 s1; return (z, s2)}
          k' z1 a1 = do {(z2, a2) <- v2 a1; b2 <- k z1 a2; u2 z2 b2}
  (//) (KleisliOptic v u) (KleisliContext h k) = KleisliContext h' k'
    where h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
          k' (z, s1) a2 = do {(_, a1) <- v s1; (_, b2) <- k z (a1, a2); return b2}

-- In the following, [] (non-empty lists) is being used as the pointed powerset monad,
-- with head as the basepoint

instance ContextAdd (KleisliContext []) where
  prl = undefined
  prr = undefined

type SubgamePerfectOpenGame = OpticOpenGame (KleisliOptic []) (KleisliContext []) Bool

subgamePerfectDecision :: (Eq x, Eq y, Ord r) => [y] -> SubgamePerfectOpenGame (x -> y) x () y r
subgamePerfectDecision ys = OpticOpenGame {
  play = \a -> KleisliOptic (\x -> zip (repeat ()) (a x : ys)) (\() _ -> [()]),
  equilibrium = \(KleisliContext h k) a -> and [head (k z (a x)) >= head (k z y) | (z, x) <- h, y <- ys]}

instance Decision (->) SubgamePerfectOpenGame where
  decision = const subgamePerfectDecision

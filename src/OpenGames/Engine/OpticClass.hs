{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpenGames.Engine.OpticClass where

-- Experimental type classes for optics and contexts

import           OpenGames.Engine.OpenGamesClass hiding (fromFunctions)
import           Data.Tuple                             (swap)

class Optic o where
  lens :: Eq s => (s -> a) -> (s -> b -> t) -> o s t a b
  (>>>>) :: o s t a b -> o a b p q -> o s t p q
  (&&&&) :: o s1 t1 a1 b1 -> o s2 t2 a2 b2 -> o (s1, s2) (t1, t2) (a1, a2) (b1, b2)
  (++++) :: o s1 t a1 b -> o s2 t a2 b -> o (Either s1 s2) t (Either a1 a2) b

identity :: (Eq s, Optic o) => o s t s t
identity = lens id (flip const)

class (Optic o) => Context c o | c -> o where
  void :: c () () () ()
  cmap :: o s1 t1 s2 t2 -> o a1 b1 a2 b2 -> c s1 t1 a2 b2 -> c s2 t2 a1 b1
  (//) :: (Show s1) => o s1 t1 a1 b1 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s2 t2 a2 b2
  (\\) :: (Show s2) => o s2 t2 a2 b2 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s1 t1 a1 b1
  l \\ c = l // cmap swapper swapper c
    where fromFunctions :: (Optic o) => (s -> a) -> (b -> t) -> o s t a b
          fromFunctions f g = lens f (const g)

          swapper = fromFunctions swap swap

-- ContextAdd is a separate class to Precontext and Context because its implementation is more ad-hoc,
-- eg. it can't be done generically in a monad

class ContextAdd c where
  prl :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s1 t a1 b)
  prr :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s2 t a2 b)

data OpticOpenGame o c m a x s y r = OpticOpenGame {
  play        :: a -> o x s y r,
  equilibrium :: c x s y r -> a -> m}

instance (Optic o, Context c o, ContextAdd c, Monoid m) => OG (OpticOpenGame o c m) where
  fromLens v u = OpticOpenGame {
    play = \() -> lens v u,
    equilibrium = \_ () -> mempty}
  reindex f g = OpticOpenGame {
    play = \a -> play g (f a),
    equilibrium = \c a -> equilibrium g c (f a)}
  (>>>) g1 g2 = OpticOpenGame {
    play = \(a, b) -> play g1 a >>>> play g2 b,
    equilibrium = \c (a, b) -> equilibrium g1 (cmap identity (play g2 b) c) a <> equilibrium g2 (cmap (play g1 a) identity c) b}
  (&&&) g1 g2 = OpticOpenGame {
    play = \(a, b) -> play g1 a &&&& play g2 b,
    equilibrium = \c (a, b) -> equilibrium g1 ((play g2 b) \\ c) a <> equilibrium g2 ((play g1 a) // c) b}
  (+++) g1 g2 = OpticOpenGame {
    play = \(a, b) -> play g1 a ++++ play g2 b,
    equilibrium = \c (a, b) -> let e1 = case prl c of {Nothing -> mempty; Just c1 -> equilibrium g1 c1 a}
                                   e2 = case prr c of {Nothing -> mempty; Just c2 -> equilibrium g2 c2 b}
                                in e1 <> e2}

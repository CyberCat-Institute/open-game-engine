{-# LANGUAGE MultiParamTypeClasses #-}

module OpenGames.Engine.OpticClass where

import Control.Applicative

class Optic o where
  lens :: (s -> a) -> (s -> b -> t) -> o s t a b
  (>>>>) :: o s t a b -> o a b p q -> o s t p q
  (&&&&) :: o s1 t1 a1 b1 -> o s2 t2 a2 b2 -> o (s1, s2) (t1, t2) (a1, a2) (b1, b2)
  (++++) :: o s1 t a1 b -> o s2 t a2 b -> o (Either s1 s2) t (Either a1 a2) b

identity :: (Optic o) => o s t s t
identity = lens id (flip const)

class Precontext c where
  void :: c () () () ()

-- Precontext is a separate class to Context because otherwise the typechecker throws a wobbly

class (Optic o, Precontext c) => Context c o where
  cmap :: o s1 t1 s2 t2 -> o a1 b1 a2 b2 -> c s1 t1 a2 b2 -> c s2 t2 a1 b1
  (//) :: (Show s1) => o s1 t1 a1 b1 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s2 t2 a2 b2
  (\\) :: (Show s2) => o s2 t2 a2 b2 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s1 t1 a1 b1

-- (\\) is derivable from (//) using
-- l \\ c = l // (cmap (lift swap swap) (lift swap swap) c)
-- (and vice versa) but it doesn't typecheck and I don't understand why

-- ContextAdd is a separate class to Precontext and Context because its implementation is more ad-hoc,
-- eg. it can't be done generically in a monad

class ContextAdd c where
  prl :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s1 t a1 b)
  prr :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s2 t a2 b)

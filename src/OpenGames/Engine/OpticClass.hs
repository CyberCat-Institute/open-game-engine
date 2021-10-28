{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, FunctionalDependencies #-}

module OpenGames.Engine.OpticClass where

class Optic ctx o | o -> ctx where
  adaptor :: (s -> a) -> (b -> t) -> o s t a b
  continuation :: ctx s => (s -> t) -> o s t () ()
  (>>>>) :: o s t a b -> o a b p q -> o s t p q
  (&&&&) :: o s1 t1 a1 b1 -> o s2 t2 a2 b2 -> o (s1, s2) (t1, t2) (a1, a2) (b1, b2)
  (++++) :: o s1 t a1 b -> o s2 t a2 b -> o (Either s1 s2) t (Either a1 a2) b

identity :: (Optic ctx o) => o s t s t
identity = adaptor id id

{-
  void ought to be a member of Context, but poor Haskell finds it tricky, so Precontext is a workaround.
  Daan Rijks pointed out that adding a (c -> o) fundep in Context makes that work in simple cases,
  - see https://github.com/jules-hedges/open-games-hs/pull/10 -
  but sadly all the extra stuff going on here makes that stop working so we're back to Precontext.
-}

class Precontext c where
  void :: c () () () ()

{-
  For details on how the Context class works, see "The game semantics of game theory",
  [todo]
-}

{-
  (\\) is derivable from (//) using
  l \\ c = l // (cmap (adaptor swap swap) (adaptor swap swap) c)
  (and vice versa) but the necessary fundep to make this work is too much for Haskell to cope with,
  for the same reasons that we need Precontext
-}

class (Optic ctx1 o, Precontext c) => Context ctx1 ctx2 o c | c -> ctx2 where
  cmap :: o s1 t1 s2 t2 -> o a1 b1 a2 b2 -> c s1 t1 a2 b2 -> c s2 t2 a1 b1
  (//) :: ctx2 s1 => o s1 t1 a1 b1 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s2 t2 a2 b2
  (\\) :: ctx2 s2 => o s2 t2 a2 b2 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s1 t1 a1 b1

{-
  ContextAdd is a separate class to Context because its implementation is more ad-hoc,
  eg. it can't be done generically in a monad
-}

class ContextAdd c where
  matchl :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s1 t a1 b)
  matchr :: c (Either s1 s2) t (Either a1 a2) b -> Maybe (c s2 t a2 b)

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

lens :: (Optic ctx o) => (s -> a) -> (s -> b -> t) -> o s t a b
lens = undefined

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
  {-

-- Experimental non Stochastic
-- Same as used in learning implementation
-- Can be used for IO as well
data MonadOptic msg s t a b where
  MonadOptic :: (s -> (RIO (GLogFunc msg)) (z, a))
                          -> (z -> b -> StateT Vector (RIO (GLogFunc msg)) t)
                          -> MonadOptic msg s t a b

instance Optic (MonadOptic msg) where
  lens v u = MonadOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where v (Left s1)  = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b  = u1 z1 b
          u (Right z2) b = u2 z2 b

data MonadContext msg s t a b where
  MonadContext :: (Show z) => (RIO (GLogFunc msg)) (z, s) -> (z -> a -> StateT Vector (RIO (GLogFunc msg)) b) -> MonadContext msg s t a b

instance Precontext (MonadContext msg) where
  void = MonadContext (return ((), ())) (\() () -> return ())

instance Context (MonadContext msg) (MonadOptic msg) where
  cmap (MonadOptic v1 u1) (MonadOptic v2 u2) (MonadContext h k)
            = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
                  k' z a = do {(z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'}
               in MonadContext h' k'
  (//) (MonadOptic v u) (MonadContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
                  k' (z, s1) a2 = do {(_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2}
               in MonadContext h' k'
  (\\) (MonadOptic v u) (MonadContext h k)
            = let h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
                  k' (z, s2) a1 = do {(_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1}
               in MonadContext h' k'
               -}

{-# LANGUAGE GADTs, ConstraintKinds, QuantifiedConstraints, StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, PolyKinds, DataKinds#-}

module OpenGames.Engine.KleisliOptics where
import Data.Constraint
import Control.Monad.Trans.Class

import OpenGames.Engine.OpticClass

-- type KleisliOptic :: (ctx :: * -> Constraint) -> (m :: * -> *) -> (mt :: (* -> *) -> * -> *) -> (s :: *) -> (t :: *) -> (a :: *) -> (b :: *) -> *
data KleisliOptic ctx m mt s t a b where
  KleisliOptic :: ctx z => (s -> m (z, a)) -> (z -> b -> mt m t) -> KleisliOptic ctx m mt s t a b

{-
  This ought to be
  type Cartesian ctx = (ctx (), forall a1 a2 . (ctx a1, ctx a2) => ctx (a1, a2))
  but we can't figure out how to make it work
-}
type Cartesian :: (* -> Constraint) -> Constraint
type Cartesian ctx = forall a1 a2 . (ctx a1, ctx a2) => ctx (a1, a2)

type Cocartesian :: (* -> Constraint) -> Constraint
type Cocartesian ctx = forall a1 a2 . (ctx a1, ctx a2) => ctx (Either a1 a2)

instance (ctx (), Cartesian ctx, Cocartesian ctx, Monad m, Monad (mt m))
      => Optic ctx (KleisliOptic ctx m mt) where
  adaptor f g = KleisliOptic v u where
    v s = return ((), f s)
    u () b = return (g b)
  continuation k = KleisliOptic v u where
    v s = return (s, ())
    u s () = return (k s)
  (>>>>) (KleisliOptic v1 u1) (KleisliOptic v2 u2) = KleisliOptic v u where
    v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
    u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (KleisliOptic v1 u1) (KleisliOptic v2 u2) = KleisliOptic v u where
    v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
    u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (KleisliOptic v1 u1) (KleisliOptic v2 u2) = KleisliOptic v u where
    v (Left s1) = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
    v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
    u (Left z1) b = u1 z1 b
    u (Right z2) b = u2 z2 b

-- type KleisliContext :: (* ~> Constraint) (m :: * -> *) (mt :: (* -> *) -> * -> *) (s :: *) (t :: *) (a :: *) (b :: *)
data KleisliContext ctx m mt s t a b where
  KleisliContext :: ctx z => m (z, s) -> (z -> a -> mt m b) -> KleisliContext ctx m mt s t a b

instance (Monad m, Monad (mt m), ctx ()) => Precontext (KleisliContext ctx m mt) where
  void = KleisliContext h k where
    h = return ((), ())
    k () () = return ()

instance (Cartesian ctx1, Cocartesian ctx1, Cartesian ctx2, ctx1 (), ctx2 (), Monad m, MonadTrans mt, Monad (mt m))
      => Context ctx1 ctx2 (KleisliOptic ctx1 m mt) (KleisliContext ctx2 m mt) where
  cmap (KleisliOptic v1 _) (KleisliOptic v2 u2) (KleisliContext h k) = KleisliContext h' k' where
    h' = do {(z, s1) <- h; (_, s2) <- v1 s1; return (z, s2)}
    k' z a2 = do {(z', a1) <- lift (v2 a2); b1 <- k z a1; u2 z' b1}
  (//) (KleisliOptic v _) (KleisliContext h k) = KleisliContext h' k' where
    h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
    k' (z, s1) a2 = do {(_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2}
  (\\) (KleisliOptic v _) (KleisliContext h k) = KleisliContext h' k' where
    h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
    k' (z, s2) a1 = do {(_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1}

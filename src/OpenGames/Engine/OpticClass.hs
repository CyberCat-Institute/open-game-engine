{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module OpenGames.Engine.OpticClass
  ( Stochastic (..),
    Vector (..),
    StochasticStatefulOptic (..),
    StochasticStatefulContext (..),
    StochasticOptic (..),
    StochasticContext (..),
    MonadOptic (..),
    MonadContext (..),
    Optic (..),
    Precontext (..),
    Context (..),
    ContextAdd (..),
    identity,
  )
where

import Control.Monad.State hiding (state)
import Data.HashMap as HM hiding (map, mapMaybe, null)
import Data.Maybe
import qualified Data.Vector as V
import Numeric.Probability.Distribution hiding (lift)
import System.Random.MWC.CondensedTable
import System.Random.Stateful

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

-------------------------------------------------------------
--- replicate the old implementation of a stochastic context
type Stochastic = T Double

type Vector = HM.Map String Double

data StochasticStatefulOptic s t a b where
  StochasticStatefulOptic ::
    (s -> Stochastic (z, a)) ->
    (z -> b -> StateT Vector Stochastic t) ->
    StochasticStatefulOptic s t a b

instance Optic StochasticStatefulOptic where
  lens v u = StochasticStatefulOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where
      v s = do (z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)
      u (z1, z2) q = do b <- u2 z2 q; u1 z1 b
  (&&&&) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where
      v (s1, s2) = do (z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))
      u (z1, z2) (b1, b2) = do t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)
  (++++) (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) = StochasticStatefulOptic v u
    where
      v (Left s1) = do (z1, a1) <- v1 s1; return (Left z1, Left a1)
      v (Right s2) = do (z2, a2) <- v2 s2; return (Right z2, Right a2)
      u (Left z1) b = u1 z1 b
      u (Right z2) b = u2 z2 b

data StochasticStatefulContext s t a b where
  StochasticStatefulContext :: (Show z) => Stochastic (z, s) -> (z -> a -> StateT Vector Stochastic b) -> StochasticStatefulContext s t a b

instance Precontext StochasticStatefulContext where
  void = StochasticStatefulContext (return ((), ())) (\() () -> return ())

instance Context StochasticStatefulContext StochasticStatefulOptic where
  cmap (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2) (StochasticStatefulContext h k) =
    let h' = do (z, s) <- h; (_, s') <- v1 s; return (z, s')
        k' z a = do (z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'
     in StochasticStatefulContext h' k'
  (//) (StochasticStatefulOptic v u) (StochasticStatefulContext h k) =
    let h' = do (z, (s1, s2)) <- h; return ((z, s1), s2)
        k' (z, s1) a2 = do (_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2
     in StochasticStatefulContext h' k'
  (\\) (StochasticStatefulOptic v u) (StochasticStatefulContext h k) =
    let h' = do (z, (s1, s2)) <- h; return ((z, s2), s1)
        k' (z, s2) a1 = do (_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1
     in StochasticStatefulContext h' k'

instance ContextAdd StochasticStatefulContext where
  prl (StochasticStatefulContext h k) =
    let fs = [((z, s1), p) | ((z, Left s1), p) <- decons h]
     in if null fs
          then Nothing
          else Just (StochasticStatefulContext (fromFreqs fs) (\z a1 -> k z (Left a1)))
  prr (StochasticStatefulContext h k) =
    let fs = [((z, s2), p) | ((z, Right s2), p) <- decons h]
     in if null fs
          then Nothing
          else Just (StochasticStatefulContext (fromFreqs fs) (\z a2 -> k z (Right a2)))

-- Experimental non state
data StochasticOptic s t a b where
  StochasticOptic ::
    (s -> Stochastic (z, a)) ->
    (z -> b -> Stochastic t) ->
    StochasticOptic s t a b

instance Optic StochasticOptic where
  lens v u = StochasticOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (StochasticOptic v1 u1) (StochasticOptic v2 u2) = StochasticOptic v u
    where
      v s = do (z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)
      u (z1, z2) q = do b <- u2 z2 q; u1 z1 b
  (&&&&) (StochasticOptic v1 u1) (StochasticOptic v2 u2) = StochasticOptic v u
    where
      v (s1, s2) = do (z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))
      u (z1, z2) (b1, b2) = do t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)
  (++++) (StochasticOptic v1 u1) (StochasticOptic v2 u2) = StochasticOptic v u
    where
      v (Left s1) = do (z1, a1) <- v1 s1; return (Left z1, Left a1)
      v (Right s2) = do (z2, a2) <- v2 s2; return (Right z2, Right a2)
      u (Left z1) b = u1 z1 b
      u (Right z2) b = u2 z2 b

data StochasticContext s t a b where
  StochasticContext :: (Show z) => Stochastic (z, s) -> (z -> a -> Stochastic b) -> StochasticContext s t a b

instance Precontext StochasticContext where
  void = StochasticContext (return ((), ())) (\() () -> return ())

instance Context StochasticContext StochasticOptic where
  cmap (StochasticOptic v1 u1) (StochasticOptic v2 u2) (StochasticContext h k) =
    let h' = do (z, s) <- h; (_, s') <- v1 s; return (z, s')
        k' z a = do (z', a') <- (v2 a); b' <- k z a'; u2 z' b'
     in StochasticContext h' k'
  (//) (StochasticOptic v u) (StochasticContext h k) =
    let h' = do (z, (s1, s2)) <- h; return ((z, s1), s2)
        k' (z, s1) a2 = do (_, a1) <- (v s1); (_, b2) <- k z (a1, a2); return b2
     in StochasticContext h' k'
  (\\) (StochasticOptic v u) (StochasticContext h k) =
    let h' = do (z, (s1, s2)) <- h; return ((z, s2), s1)
        k' (z, s2) a1 = do (_, a2) <- (v s2); (b1, _) <- k z (a1, a2); return b1
     in StochasticContext h' k'

instance ContextAdd StochasticContext where
  prl (StochasticContext h k) =
    let fs = [((z, s1), p) | ((z, Left s1), p) <- decons h]
     in if null fs
          then Nothing
          else Just (StochasticContext (fromFreqs fs) (\z a1 -> k z (Left a1)))
  prr (StochasticContext h k) =
    let fs = [((z, s2), p) | ((z, Right s2), p) <- decons h]
     in if null fs
          then Nothing
          else Just (StochasticContext (fromFreqs fs) (\z a2 -> k z (Right a2)))

-- Experimental non Stochastic
-- Same as used in learning implementation
-- Can be used for IO as well
data MonadOptic s t a b where
  MonadOptic ::
    (s -> IO (z, a)) ->
    (z -> b -> StateT Vector IO t) ->
    MonadOptic s t a b

instance Optic MonadOptic where
  lens v u = MonadOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where
      v s = do (z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)
      u (z1, z2) q = do b <- u2 z2 q; u1 z1 b
  (&&&&) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where
      v (s1, s2) = do (z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))
      u (z1, z2) (b1, b2) = do t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)
  (++++) (MonadOptic v1 u1) (MonadOptic v2 u2) = MonadOptic v u
    where
      v (Left s1) = do (z1, a1) <- v1 s1; return (Left z1, Left a1)
      v (Right s2) = do (z2, a2) <- v2 s2; return (Right z2, Right a2)
      u (Left z1) b = u1 z1 b
      u (Right z2) b = u2 z2 b

data MonadContext s t a b where
  MonadContext :: (Show z) => IO (z, s) -> (z -> a -> StateT Vector IO b) -> MonadContext s t a b

instance Precontext MonadContext where
  void = MonadContext (return ((), ())) (\() () -> return ())

instance Context MonadContext MonadOptic where
  cmap (MonadOptic v1 u1) (MonadOptic v2 u2) (MonadContext h k) =
    let h' = do (z, s) <- h; (_, s') <- v1 s; return (z, s')
        k' z a = do (z', a') <- lift (v2 a); b' <- k z a'; u2 z' b'
     in MonadContext h' k'
  (//) (MonadOptic v u) (MonadContext h k) =
    let h' = do (z, (s1, s2)) <- h; return ((z, s1), s2)
        k' (z, s1) a2 = do (_, a1) <- lift (v s1); (_, b2) <- k z (a1, a2); return b2
     in MonadContext h' k'
  (\\) (MonadOptic v u) (MonadContext h k) =
    let h' = do (z, (s1, s2)) <- h; return ((z, s2), s1)
        k' (z, s2) a1 = do (_, a2) <- lift (v s2); (b1, _) <- k z (a1, a2); return b1
     in MonadContext h' k'

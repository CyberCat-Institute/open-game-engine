{-# LANGUAGE GADTs, DataKinds, FlexibleInstances, ConstraintKinds, KindSignatures, RankNTypes, StandaloneKindSignatures #-}

module OpenGames.Engine.Pure where

import Data.Constraint
import Data.Singletons
import Data.Functor.Identity
import Control.Monad.Trans.Identity

import OpenGames.Engine.KleisliOptics
import OpenGames.Engine.TLL
import OpenGames.Engine.OpenGames

class Empty a where

instance Empty a where

decision :: [t] -> OpenGame (KleisliOptic Empty Identity IdentityT)
                   (KleisliContext Empty Identity IdentityT)
                   '[s -> t] '[Bool]
                   s () t Double
decision ts = OpenGame {
  play = \(f ::- Nil) -> let v s = Identity ((), f s)
                             u () _ = IdentityT (Identity ())
                          in KleisliOptic v u,
  evaluate = \(f ::- Nil) (KleisliContext (Identity (z, h)) k) ->
    let k' = runIdentity . runIdentityT . k z
     in all (\t -> k' (f h) >= k' t) ts ::- Nil
}

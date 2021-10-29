{-# LANGUAGE GADTs, DataKinds, ConstraintKinds, KindSignatures, RankNTypes, StandaloneKindSignatures #-}

module OpenGames.Engine.Pure where

import Data.Constraint
import Data.Singletons
import Data.Functor.Identity
import Control.Monad.Trans.Identity

import OpenGames.Engine.KleisliOptics
import OpenGames.Engine.TLL
import OpenGames.Engine.OpenGames

{-}
type Empty :: * -> Constraint
type Empty a = (() :: Constraint)
-}

-- type Empty :: Constraint
-- type Empty = ()

decision :: [t] -> OpenGame (KleisliOptic Empty Identity IdentityT)
                   (KleisliContext Empty Identity IdentityT)
                   '[s -> t] '[Bool]
                   s () t Double
decision ts = OpenGame {
  play = \(f :- Nil) -> let v s = Identity ((), f s)
                            u () _ = IdentityT (Identity ())
                         in KleisliOptic v u,
  evaluate = \(f :- Nil) (KleisliContext (Identity (z, h)) k) ->
    let k' = runIdentity . runIdentityT . k z
     in all (\t -> k' (f h) >= k' t) ts :- Nil
}

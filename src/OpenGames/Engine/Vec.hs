{-# LANGUAGE KindSignatures, DataKinds, GADTs, PolyKinds,
             TypeOperators, TypeFamilies #-}

module OpenGames.Engine.Vec where

import OpenGames.Engine.Nat
import Prelude hiding (map, replicate)

infixr 6 :>
data Vec (n :: Nat) (t :: *) where
  Empty :: Vec n a
  (:>) :: t -> Vec n t -> Vec (S n) t

-- Given a function from a to b, map a vector preserving its length
map :: (a -> b) -> Vec n a -> Vec n b
map f Empty = Empty
map f (x :> xs) = f x :> map f xs

-- given a nat, generate the list of numbers for it starting with 0
enumerate :: Natural n -> Vec n Int
enumerate Zero = Empty
enumerate (Succ n) = 0 :> map (1+) (enumerate n)

-- replicate an element `n` times into a vector
replicate :: Natural n -> a -> Vec n a
replicate Zero _ = Empty
replicate (Succ n) a = a :> replicate n a

-- given an element, make a non-empty vector containing that element
mkVec :: a -> Vec (S Z) a
mkVec a = a :> Empty

-- Given a non-empty vector, get its first element
vecHead :: Vec (S n) a -> a
vecHead (x :> _) = x

{-# LANGUAGE KindSignatures, DataKinds, GADTs, TypeFamilies #-}

module OpenGames.Engine.Nat where

import GHC.Types (Type)

--  Natural numbers as a unary data type
data Nat = Z | S Nat

-- Singleton type for natural numbers
data Natural (n :: Nat) where
  Zero :: Natural Z
  Succ :: Natural n -> Natural (S n)

-- Converts a TNat to its `int` value, an O(n) operation
natToInt :: Natural n -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n


type family Add (n :: Nat) (m :: Nat) :: Nat where

type family Flatten (a :: Type) (b :: Type) :: Type where
  Flatten () b = b
  Flatten (a, b) c = (a, Flatten b c)

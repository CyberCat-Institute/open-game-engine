{-# LANGUAGE KindSignatures, DataKinds, GADTs #-}

module OpenGames.Engine.Nat where

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


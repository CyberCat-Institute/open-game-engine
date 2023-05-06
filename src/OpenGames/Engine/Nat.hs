{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module OpenGames.Engine.Nat where

--  Natural numbers as a unary data type
data Nat = Z | S Nat

-- Singleton type for natural numbers
data Natural (n :: Nat) where
  Zero :: Natural Z
  Succ :: Natural n -> Natural (S n)

minus :: Nat -> Nat -> Nat
minus = undefined

plus :: Nat -> Nat -> Nat
plus = undefined

-- Converts a TNat to its `int` value, an O(n) operation
natToInt :: Natural n -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

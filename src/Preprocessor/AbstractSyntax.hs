{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Preprocessor.AbstractSyntax
  ( Line(..)
  , pureLine
  , mkLine
  , Block(..)
  ) where

import Control.Comonad
import Data.Bifunctor

-- The user interacts with the preprocessor by creating instances of the datatypes in this file
-- and then calling functions from Compiler on it

-- The only reason there is no concrete syntax is that I have no idea how to write a parser
-- Somebody can probably fix that in half an hour
-- My idea for the concrete syntax of a line is
-- cvo, ..., cvo' | cno, ..., cno' <- matrix -< cvi, ..., cvi' | cni, ..., cvi'

-- covariant input = X, covariant output = Y, contravariant input = R, contravariant output = S

-- There is an important duality that the types can't express: half of these are lists of Haskell variables
-- (they could probably be patterns) that create new bindings, and half of them are lists of Haskell expressions
-- Line outputs and block inputs are variables/patterns, line inputs and block outputs are expressions

-- Variables/patterns: covariantOutput, contravariantOutput, blockCovariantInput, blockContravariantInput
-- Expressions:        covariantInput, contravariantInput, blockCovariantOutput, blockContravariantOutput

-- I decided to keep the record field names verbose, and I expect the user to specify lines in constructor syntax
-- rather than record syntax

data Line l p e = Line {
  label :: l,
  covariantInputs :: [e], contravariantOutputs :: [p],
  matrix :: e, --
  covariantOutputs :: [p], contravariantInputs :: [e]} deriving (Eq, Show, Functor)

mkLine :: [e] -> [p] -> e -> [p] -> [e] -> Line (Maybe l) p e
mkLine = Line Nothing


instance Comonad (Line (Maybe l) p) where
  extract (Line _ _ _ e _ _) = e
  extend f v = pure (f v)

instance Bifunctor (Line l) where
  first f (Line lbl covi cono m covo coni) =
    Line lbl covi (fmap f cono) m (fmap f covo) coni
  second = fmap

pureLine :: forall l p a. a -> Line (Maybe l) p a
pureLine v = Line Nothing [] [] v [] []

instance Applicative (Line (Maybe l) p) where
  pure = pureLine
  (Line _ _ _ f _ _) <*> (Line label covIn conOut m covOut conIn) =
    Line label (fmap f covIn) conOut (f m) covOut (fmap f conIn)

instance Foldable (Line l p) where
  foldr f init (Line _ _ _ arg _ _)  = f arg init

instance Traversable (Line l p) where
  traverse f (Line lbl covIn conOut m covOut conIn) =
    pure (Line lbl) <*> traverse f covIn
                    <*> pure conOut
                    <*> f m
                    <*> pure covOut
                    <*> traverse f conIn

data Block p e = Block {
  blockCovariantInputs :: [p], blockContravariantOutputs :: [e],
  blockLines :: [Line (Maybe String) p e],
  blockCovariantOutputs :: [e], blockContravariantInputs :: [p]} deriving (Eq, Show, Functor)


instance Applicative (Block p) where
  pure v = Block [] [] (pure (pure v)) [] []
  (<*>) :: Block p (a -> b) -> Block p a -> Block p b
  (Block _ _ f _ _) <*> (Block covIn conOut m covOut conIn) =
    let v = fmap (<*>) f in
    Block covIn
          (mapLines f conOut)
          (fmap (<*>) f <*> m)
          (mapLines f covOut)
          conIn
      where
        mapLines :: [Line (Maybe String) p (a -> b)] -> [a] -> [b]
        mapLines f as = fmap extract f <*> as


instance Foldable (Block p) where
  foldr f init (Block _ _ arg _ _)  =
    foldr (\l b -> foldr f b l) init arg

instance Traversable (Block p) where
  traverse f (Block covi cono l covo coni) =
    pure Block <*> pure covi
               <*> traverse f cono
               <*> traverse (traverse f) l
               <*> traverse f covo
               <*> pure coni

instance Bifunctor Block where
  first f (Block covi cono l covo coni) =
    Block (fmap f covi) cono (fmap (first f) l) covo (fmap f coni)
  second = fmap

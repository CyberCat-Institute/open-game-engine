{-# LANGUAGE TemplateHaskell #-}
module Act.TH.State where

import Act.Utils

import Data.Bifunctor

import Syntax.Annotated

import qualified Data.Map as M

import EVM.Solidity (SlotType)

import Language.Haskell.TH.Syntax

-- Generate a type for the global state of the contract
stateDec4Claim :: [Claim] -> [Dec]
stateDec4Claim = concatMap convertStorage
    where
        convertStorage :: Claim -> [Dec]
        convertStorage (C c) = []
        convertStorage (B b) = []
        convertStorage (I i) = []
        convertStorage (S s) = fmap (createDataDeclaration . second M.toList) (M.toList s)

createDataDeclaration :: (Id, [(Id, SlotType)]) -> Dec
createDataDeclaration (storeName, types) = DataD
  [] -- no constraints
  (storeTypeName) -- the name is the name from the store
  [] -- no type variables
  Nothing -- no kind signature, not a GADT
  [RecC storeTypeName (fmap (\(nm, ty) -> (mkName nm, defaultBang, mapEVMTypes ty)) types)]
  [DerivClause Nothing [ConT ''Show, ConT ''Eq]]
  where
    storeTypeName :: Name
    storeTypeName = mkName (capitalise (storeName ++ "State"))

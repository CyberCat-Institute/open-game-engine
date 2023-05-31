{-# LANGUAGE TemplateHaskell #-}

module Act.TH.State where

import Act.Utils
import Data.Bifunctor
import qualified Data.Map as M
import Language.Haskell.TH.Syntax
import Syntax.Annotated (Id, Store, SlotType)

-- Generate a type for the global state of the contract
stateDec4Interface :: Store -> [Dec]
stateDec4Interface = fmap createDataDeclaration . M.toList

createDataDeclaration :: (Id, M.Map Id SlotType) -> Dec
createDataDeclaration (storeName, types) =
  DataD
    [] -- no constraints
    (storeTypeName storeName) -- the name is the name from the store
    [] -- no type variables
    Nothing -- no kind signature, not a GADT
    [RecC (storeTypeName storeName) (fmap (\(nm, ty) -> (mkName nm, defaultBang, mapEVMTypes ty)) (M.toList types))]
    [DerivClause Nothing [ConT ''Show, ConT ''Eq]]

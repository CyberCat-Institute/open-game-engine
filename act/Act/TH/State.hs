{-# LANGUAGE TemplateHaskell #-}

module Act.TH.State where

import Act.Utils
import Data.Bifunctor
import qualified Data.Map as M
import Language.Haskell.TH.Syntax
import Syntax.Untyped (Creates(..), SlotType)
import Syntax.Annotated (Id)

-- Generate a type for the global state of the contract
stateDec4Claim :: Creates -> [Dec]
stateDec4Claim (Creates assign) = [createDataDeclaration undefined]
--   where
--     convertStorage :: Claim -> [Dec]
--     convertStorage (C c) = []
--     convertStorage (B b) = []
--     convertStorage (I i) = []
--     convertStorage (S s) = fmap (createDataDeclaration . second M.toList) (M.toList s)

createDataDeclaration :: (Id, [(Id, SlotType)]) -> Dec
createDataDeclaration (storeName, types) =
  DataD
    [] -- no constraints
    (storeTypeName) -- the name is the name from the store
    [] -- no type variables
    Nothing -- no kind signature, not a GADT
    [RecC storeTypeName (fmap (\(nm, ty) -> (mkName nm, defaultBang, mapEVMTypes ty)) types)]
    [DerivClause Nothing [ConT ''Show, ConT ''Eq]]
  where
    storeTypeName :: Name
    storeTypeName = mkName (capitalise (storeName ++ "State"))

{-# LANGUAGE TemplateHaskell #-}
module Act.TH.State where

import Act.Utils

import Data.Bifunctor

import EVM.Solidity

import Syntax.Annotated

import qualified Data.Map as M

import Language.Haskell.TH.Syntax

mapEVMTypes2HS :: SlotType -> Type
mapEVMTypes2HS (StorageMapping _ _) = undefined
mapEVMTypes2HS (StorageValue (AbiUIntType n)) = ConT ''Int
mapEVMTypes2HS (StorageValue (AbiIntType n)) = ConT ''Int
mapEVMTypes2HS (StorageValue AbiAddressType) = undefined
mapEVMTypes2HS (StorageValue AbiBoolType) = ConT ''Bool
mapEVMTypes2HS (StorageValue (AbiBytesType n)) = undefined
mapEVMTypes2HS (StorageValue AbiBytesDynamicType) = undefined
mapEVMTypes2HS (StorageValue AbiStringType) = undefined
mapEVMTypes2HS (StorageValue (AbiArrayDynamicType ty)) = undefined
mapEVMTypes2HS (StorageValue (AbiArrayType n ty)) = undefined
mapEVMTypes2HS (StorageValue (AbiTupleType _)) = undefined


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
  [RecC storeTypeName (fmap (\(nm, ty) -> (mkName nm, defaultBang, mapEVMTypes2HS ty)) types)]
  [] -- no derived clauses, for now. Might be useful to have Eq etc defined
  where
    defaultBang :: Bang
    defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

    storeTypeName :: Name
    storeTypeName = mkName (capitalise (storeName ++ "State"))

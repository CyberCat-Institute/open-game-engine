{-# LANGUAGE TemplateHaskell #-}
module Act.Utils where

import Data.Char (toUpper, toLower)

import Language.Haskell.TH.Syntax

import EVM.Solidity
import EVM.ABI

import Data.Text (Text)

mapEVMTypes :: SlotType -> Type
mapEVMTypes (StorageMapping _ _) = undefined
mapEVMTypes (StorageValue (AbiUIntType n)) = ConT ''Int
mapEVMTypes (StorageValue (AbiIntType n)) = ConT ''Int
mapEVMTypes (StorageValue AbiAddressType) = undefined
mapEVMTypes (StorageValue AbiBoolType) = ConT ''Bool
mapEVMTypes (StorageValue (AbiBytesType n)) = undefined
mapEVMTypes (StorageValue AbiBytesDynamicType) = undefined
mapEVMTypes (StorageValue AbiStringType) = undefined
mapEVMTypes (StorageValue (AbiArrayDynamicType ty)) = undefined
mapEVMTypes (StorageValue (AbiArrayType n ty)) = undefined
mapEVMTypes (StorageValue (AbiTupleType _)) = undefined

mapAbiTypes :: AbiType -> Type
mapAbiTypes (AbiUIntType n) = ConT ''Int
mapAbiTypes (AbiIntType n) = ConT ''Int
mapAbiTypes AbiAddressType = undefined
mapAbiTypes AbiBoolType = ConT ''Bool
mapAbiTypes (AbiBytesType n) = undefined
mapAbiTypes AbiBytesDynamicType = undefined
mapAbiTypes AbiStringType = ConT ''Text
mapAbiTypes (AbiArrayDynamicType ty) = undefined
mapAbiTypes (AbiArrayType n ty) = undefined
mapAbiTypes (AbiTupleType vty) = undefined

capitalise :: String -> String
capitalise [] = []
capitalise (x : xs) = toUpper x : xs

uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (x : xs) = toLower x : xs

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

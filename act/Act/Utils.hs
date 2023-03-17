{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Act.Utils where

import Data.Char (toUpper, toLower)

import Language.Haskell.TH.Syntax as TH

import EVM.Solidity
import EVM.ABI

import Data.Text (Text)

import Syntax.Annotated as ACT

getDeclId :: Decl -> String
getDeclId (Decl _ name) = name

getDeclType :: Decl -> AbiType
getDeclType (Decl ty _) = ty

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

mapExp :: ACT.Exp t -> Q TH.Exp
-- mapExp _ = VarE (mkName "undefined")
mapExp (And _ x y)   = [| $(mapExp x) && $(mapExp y)|]
mapExp (Or _ x y)    = [| $(mapExp x) || $(mapExp y)|]
mapExp (Impl _ x y)  = [| not $(mapExp x) || $(mapExp y)|]
mapExp (Neg _ x)     = [| not $(mapExp x) |]
mapExp (LE _ x y)    = [| $(mapExp x) < $(mapExp y) |]
mapExp (LEQ _ x y)   = [| $(mapExp x) <= $(mapExp y) |]
mapExp (GEQ _ x y)   = [| $(mapExp x) >= $(mapExp y) |]
mapExp (GE _ x y)    = [| $(mapExp x) > $(mapExp y) |]
mapExp (LitBool _ x) = [| x |]

-- integers, double check all this!
mapExp (Add _ x y) = [| $(mapExp x) +     $(mapExp y) |]
mapExp (Sub _ x y) = [| $(mapExp x) -     $(mapExp y) |]
mapExp (Mul _ x y) = [| $(mapExp x) *     $(mapExp y) |]
mapExp (Div _ x y) = [| $(mapExp x) `div` $(mapExp y) |]
mapExp (Mod _ x y) = [| $(mapExp x) `mod` $(mapExp y) |]
mapExp (Exp _ x y) = [| $(mapExp x) **    $(mapExp y) |]
mapExp (LitInt _ x) = [| x |]
mapExp (IntEnv _ x) = error "unimplemented"

-- bounds
mapExp (IntMin  _ x) = error "unimplemented"
mapExp (IntMax  _ x) = error "unimplemented"
mapExp (UIntMin _ x) = error "unimplemented"
mapExp (UIntMax _ x) = error "unimplemented"

-- bytestrings
mapExp (Cat _ x y)     = error "unimplemented"
mapExp (Slice _ a x y) = error "unimplemented"
mapExp (ByStr _ str) = error "unimplemented"
mapExp (ByLit _ byt) = error "unimplemented"
mapExp (ByEnv _ eth) = error "unimplemented"

-- builtins
mapExp (NewAddr _ x y) = error "unimplemented"

-- polymorphic
mapExp (Eq _ x y)  = [| $(mapExp x) == $(mapExp y) |]
mapExp (NEq _ x y) = [| $(mapExp x) /= $(mapExp y) |]
mapExp (ITE _ condition _then _else) = [| if $(mapExp condition) then $(mapExp _then) else $(mapExp _else) |]
mapExp (Var _ _ id) = pure (VarE $ mkName id)
mapExp (TEntry _ _ (Item _ _ storage _)) = pure (AppE (VarE $ mkName storage) (VarE (mkName "contractState")) )

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Act.Utils where

import Data.Char (toLower, toUpper)
import Data.DoubleWord (Word256)
import Data.Vector (Vector)
import EVM.ABI
import EVM.Types (Addr)
import Language.Haskell.TH.Syntax as TH
import Syntax.Annotated as ACT

storeTypeName :: String -> Name
storeTypeName storeName = mkName (capitalise (storeName ++ "State"))

getDeclId :: Decl -> String
getDeclId (Decl _ name) = name

getDeclType :: Decl -> AbiType
getDeclType (Decl ty _) = ty

mapEVMTypes :: SlotType -> Type
mapEVMTypes (StorageMapping _ _) = undefined
mapEVMTypes (StorageValue (ContractType contractRef)) = undefined -- fill up to get the storage of a contract reference
mapEVMTypes (StorageValue (PrimitiveType (AbiUIntType n))) = ConT ''Word256
mapEVMTypes (StorageValue (PrimitiveType (AbiIntType n))) = ConT ''Int
mapEVMTypes (StorageValue (PrimitiveType AbiAddressType)) = undefined
mapEVMTypes (StorageValue (PrimitiveType AbiBoolType)) = ConT ''Bool
mapEVMTypes (StorageValue (PrimitiveType (AbiBytesType n))) = undefined
mapEVMTypes (StorageValue (PrimitiveType AbiBytesDynamicType)) = undefined
mapEVMTypes (StorageValue (PrimitiveType AbiStringType)) = undefined
mapEVMTypes (StorageValue (PrimitiveType (AbiArrayDynamicType ty))) = undefined
mapEVMTypes (StorageValue (PrimitiveType (AbiArrayType n ty))) = undefined
mapEVMTypes (StorageValue (PrimitiveType (AbiTupleType _))) = undefined
mapEVMTypes (StorageValue (PrimitiveType AbiFunctionType)) = undefined

mapAbiTypes :: AbiType -> Type
mapAbiTypes (AbiUIntType n) = ConT ''Word256
mapAbiTypes (AbiIntType n) = ConT ''Int
mapAbiTypes AbiAddressType = ConT ''Addr
mapAbiTypes AbiBoolType = ConT ''Bool
mapAbiTypes (AbiBytesType n) = ConT ''ByteString
mapAbiTypes AbiBytesDynamicType = ConT ''ByteString
mapAbiTypes AbiStringType = ConT ''ByteString
mapAbiTypes (AbiArrayDynamicType ty) = ConT ''Vector `AppT` ConT ''AbiType
mapAbiTypes (AbiArrayType n ty) = ConT ''Vector `AppT` ConT ''AbiType
mapAbiTypes (AbiTupleType vty) = ConT ''Vector `AppT` ConT ''AbiType
mapAbiTypes (AbiFunctionType) = error "function types not supported"

capitalise :: String -> String
capitalise [] = []
capitalise (x : xs) = toUpper x : xs

uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (x : xs) = toLower x : xs

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness NoSourceStrictness

-- todo: Complete this function
accessStorage :: ACT.StorageRef -> TH.Exp
accessStorage (SVar _ _ varName) = VarE $ mkName varName
accessStorage (SMapping _ _ _) = error "contract storage access unimplemented"
accessStorage (SField _ _ _ _) = error "contract storage field access unimplemented"

mapExp :: ACT.Exp t -> Q TH.Exp
-- mapExp _ = VarE (mkName "undefined")
mapExp (And _ x y) = [|$(mapExp x) && $(mapExp y)|]
mapExp (Or _ x y) = [|$(mapExp x) || $(mapExp y)|]
mapExp (Impl _ x y) = [|not $(mapExp x) || $(mapExp y)|]
mapExp (Neg _ x) = [|not $(mapExp x)|]
mapExp (ACT.LT _ x y) = [|$(mapExp x) < $(mapExp y)|]
mapExp (LEQ _ x y) = [|$(mapExp x) <= $(mapExp y)|]
mapExp (GEQ _ x y) = [|$(mapExp x) >= $(mapExp y)|]
mapExp (ACT.GT _ x y) = [|$(mapExp x) > $(mapExp y)|]
mapExp (LitBool _ x) = [|x|]
-- integers, double check all this!
mapExp (Add _ x y) = [|$(mapExp x) + $(mapExp y)|]
mapExp (Sub _ x y) = [|$(mapExp x) - $(mapExp y)|]
mapExp (Mul _ x y) = [|$(mapExp x) * $(mapExp y)|]
mapExp (Div _ x y) = [|$(mapExp x) `div` $(mapExp y)|]
mapExp (Mod _ x y) = [|$(mapExp x) `mod` $(mapExp y)|]
mapExp (Exp _ x y) = [|$(mapExp x) ** $(mapExp y)|]
mapExp (LitInt _ x) = [|x|]
mapExp (IntEnv _ x) = error "unimplemented"
-- bounds
mapExp (IntMin _ x) = error "unimplemented"
mapExp (IntMax _ x) = error "unimplemented"
mapExp (UIntMin _ x) = error "unimplemented"
mapExp (UIntMax _ x) = error "unimplemented"
-- bytestrings
mapExp (Cat _ x y) = error "unimplemented"
mapExp (Slice _ a x y) = error "unimplemented"
mapExp (ByStr _ str) = error "unimplemented"
mapExp (ByLit _ byt) = error "unimplemented"
mapExp (ByEnv _ eth) = error "unimplemented"
-- polymorphic
mapExp (Eq _ _ x y) = [|$(mapExp x) == $(mapExp y)|]
mapExp (NEq _ _ x y) = [|$(mapExp x) /= $(mapExp y)|]
mapExp (ITE _ condition _then _else) = [|if $(mapExp condition) then $(mapExp _then) else $(mapExp _else)|]
mapExp (Var _ _ id) = pure (VarE $ mkName id)
mapExp (TEntry _ _ (Item _ _ storage)) = pure (AppE (accessStorage storage) (VarE (mkName "contractState")))
mapExp (Create _ _ _ _) = error "create unimplemented"

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Act.TH where

import Data.Bifunctor
import Data.Char (toUpper)
import Data.FileEmbed
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.Map as M
import Data.Validation
import Error
import EVM.Solidity

import Parse
import Lex
import Syntax.Annotated
import CLI

import Language.Haskell.TH.Syntax


act2OG :: String -> Q [Dec]
act2OG filename = do
  file <- embedFile filename
  let compiled :: Error String [Claim]
                = (compile $ unpack actSource)
  case compiled of
    Failure err -> reportError (extractError err)
                >> pure []
    Success val -> pure (concatMap dec4Claim val)

  where
  extractError :: NonEmpty (Pn, String) -> String
  extractError err = ""

dec4Claim :: Claim -> [Dec]
dec4Claim (C c) = []
dec4Claim (B b) = []
dec4Claim (I i) = []
dec4Claim (S s) = fmap (createDataDeclaration . second M.toList) (M.toList s)

actSource :: Data.ByteString.ByteString
actSource = $(embedFile "amm.act")

compiled :: Error String [Claim]
compiled =  compile (unpack actSource)

display ::  String
display = case compiled of
  Success s -> unlines (fmap printClaim s)

printClaim :: Claim -> String
printClaim (C constructor) = "constructor"
printClaim (B b) = "behaviour: " ++ unlines (fmap show (_preconditions b))
printClaim (I i) = "invariant: " ++ show i
printClaim (S s) = "Storage: " ++ show s

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

    capitalise :: String -> String
    capitalise [] = []
    capitalise (x : xs) = toUpper x : xs

    storeTypeName :: Name
    storeTypeName = mkName (capitalise storeName)

printConstructor :: Constructor -> String
printConstructor (Constructor
  name
  mode
  interface
  preconditions
  postconditions
  initialStorage
  stateupdates) =
  "constructor: " ++ name

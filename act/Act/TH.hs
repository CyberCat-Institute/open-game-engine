{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Act.TH where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed
import Data.List (nub)
import qualified Data.Map as M
import Data.Validation
import Error
import EVM.Solidity

import Parse
import Lex
import Syntax.Annotated
import CLI

import Act.Utils
import Act.TH.Dispatch

import Language.Haskell.TH.Syntax


-- Convert from a an act filepath to a list of top-level declaration
-- state is converted into a record
-- invariants are converted into if-statements
-- We make one data declaration with one constructor for each method
-- which we then use for dispatching the correct functionality in the
-- contract.
-- a single top-level function is created taking in argument
-- the method dispatcher
act2OG :: String ->  Q [Dec]
act2OG filename = do
  file <- embedFile filename
  let compiled :: Error String [Claim]
                = (compile $ unpack actSource)
  case compiled of
    Failure err -> reportError (extractError err)
                >> pure []
    -- A parsed Act file is a list of claims
    Success val -> pure (stateDec4Claim val ++ [dispatchDec4Claims val])

  where
  extractError :: NonEmpty (Pn, String) -> String
  extractError err = ""

-- Generate a type for the global state of the contract
stateDec4Claim :: [Claim] -> [Dec]
stateDec4Claim = concatMap convertStorage
    where
        convertStorage :: Claim -> [Dec]
        convertStorage (C c) = []
        convertStorage (B b) = []
        convertStorage (I i) = []
        convertStorage (S s) = fmap (createDataDeclaration . second M.toList) (M.toList s)

actSource :: Data.ByteString.ByteString
actSource = $(embedFile "amm.act")

compiled :: Error String [Claim]
compiled =  compile (unpack actSource)

display ::  String
display = case compiled of
  Success s -> unlines (fmap ((++ "\n") . printClaim) s)

printBehaviour :: Behaviour -> String
printBehaviour b =
   "name: " ++ _name b ++ "\n" ++
   "preconditions:\n" ++
   unlines (fmap (("  - " ++) . show) (_preconditions b)) ++
   "\nstate updates:\n" ++
   unlines (fmap (("  - " ++) . show) (_stateUpdates b))

printClaim :: Claim -> String
printClaim (C constructor) = "constructor: " ++ show constructor
printClaim (B b) = "behaviour:\n" ++ printBehaviour b
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

    storeTypeName :: Name
    storeTypeName = mkName (capitalise (storeName ++ "State"))

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

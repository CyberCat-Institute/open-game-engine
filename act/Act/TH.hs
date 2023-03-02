{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Act.TH where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed
import Data.Validation
import Error

import Syntax.Annotated
import CLI

import Act.TH.Dispatch
import Act.TH.State

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

-- generate a top-level function that will define the contract
-- it's type will always be the same:
-- (ContractState, ContractMethod) -> ContractState
-- This is then going to be instanciated as an open game using `fromFunctions`
generateFunction :: [Behaviour] -> Dec
generateFunction behaviors = undefined

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
   "mode: " ++ show (_mode b) ++ "\n" ++
   "preconditions:\n" ++
   unlines (fmap (("  - " ++) . show) (_preconditions b)) ++
   "\nstate updates:\n" ++
   unlines (fmap (("  - " ++) . show) (_stateUpdates b))

printClaim :: Claim -> String
printClaim (C constructor) = "constructor: " ++ show constructor
printClaim (B b) = "behaviour:\n" ++ printBehaviour b
printClaim (I i) = "invariant: " ++ show i
printClaim (S s) = "Storage: " ++ show s


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

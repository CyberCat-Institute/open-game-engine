{-# LANGUAGE TupleSections #-}
module Act.TH.Dispatch where

import Data.Bifunctor
import Data.List (nub)

import Syntax.Annotated

import Act.Utils

import Language.Haskell.TH.Syntax

-- get the type declaration for the interface of the contract
dispatchDec4Claims :: [Claim] -> Dec
dispatchDec4Claims = dec4Methods . second findMethods . extractBehaviors

-- partial because we always expect to find and `Constructor` block within a contract
findContractName :: [Claim] -> String
findContractName (C i : xs) = _cname i
findContractName (_ : xs) = findContractName xs

-- get the list of possible behaviors and the name of the contract
-- Since a program is a list of "unrelated" claims we need to find the one
-- that contains the name of the contract and the list of others that are
-- behaviours
extractBehaviors :: [Claim] -> (String, [Behaviour])
extractBehaviors claims = (findContractName claims, [bhs | (B bhs) <- claims])

-- get the name of the different "interface" declarations
-- because we need to weed out duplicate names
-- We return the method name along with a list of types which are the argument
-- needed to run the method
findMethods :: [Behaviour] -> [Interface]
findMethods = nub . fmap _interface

-- Generate a data type with the name of the contract as name type
-- and the name of each interface as inhabitants. This will be used
-- as "method dispatch"
dec4Methods :: (String, [Interface]) -> Dec
dec4Methods (tyName, constructors) = DataD
  [] -- no constraints
  (mkName $ capitalise (tyName ++ "Method")) -- the name is the name from the store
  [] -- no type variables
  Nothing -- no kind signature, not a GADT
  (fmap interface2Constructor constructors)
  [] -- no derived clauses, for now. Might be useful to have Eq etc defined
  where
    interface2Constructor :: Interface -> Con
    interface2Constructor (Interface name args) =
      NormalC (mkName (capitalise name)) (fmap ((defaultBang,) . mapAbiTypes . declType) args)

    declType :: Decl -> AbiType
    declType (Decl ty name) = ty



module Act.TH.Dispatch where

import Data.Bifunctor
import Data.List (nub)

import Syntax.Annotated

import Act.Utils

import Language.Haskell.TH.Syntax

-- get the type declaration for the interface of the contract
dispatchDec4Claims :: [Claim] -> Dec
dispatchDec4Claims = dec4Methods . second findMethods . extractBehaviors

-- get the list of possible behaviors and the name of the contract
-- Since a program is a list of "unrelated" claims we need to find the one
-- that contains the name of the contract and the list of others that are
-- behaviours
extractBehaviors :: [Claim] -> (String, [Behaviour])
extractBehaviors claims = (findName claims, [bhs | (B bhs) <- claims])
  where
  -- partial because we always expect to find and `Constructor` block within a contract
  findName :: [Claim] -> String
  findName (C i : xs) = _cname i
  findName (_ : xs) = findName xs

-- get the name of the different "interface" declarations
-- because we need to weed out duplicate names
findMethods :: [Behaviour] -> [String]
findMethods = nub . fmap _name

-- Generate a data type with the name of the contract as name type
-- and the name of each interface as inhabitants. This will be used
-- as "method dispatch"
dec4Methods :: (String, [String]) -> Dec
dec4Methods (tyName, constructors) = DataD
  [] -- no constraints
  (mkName $ capitalise (tyName ++ "Method")) -- the name is the name from the store
  [] -- no type variables
  Nothing -- no kind signature, not a GADT
  (fmap (flip NormalC [] . mkName . capitalise) constructors)
  [] -- no derived clauses, for now. Might be useful to have Eq etc defined

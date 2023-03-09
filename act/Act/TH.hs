{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Act.TH where

import Data.List
import Data.Char
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed
import Data.Validation
import Error

import Syntax.Annotated
import CLI

import Act.TH.Dispatch
import Act.TH.State
import Act.Utils
import Act.Prelude

import Language.Haskell.TH.Syntax as TH


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
    Success val -> do
      contractFunction <- generateContractFunction val
      pure (stateDec4Claim val
                     ++ [dispatchDec4Claims val]
                     ++ contractFunction)

  where
  extractError :: NonEmpty (Pn, String) -> String
  extractError err = ""

arr x y = AppT (AppT ArrowT x) y

-- generate a top-level function that will define the contract
-- it's type will always be the same:
-- (ContractState, ContractMethod) -> ContractState
-- This is then going to be instanciated as an open game using `fromFunctions`
generateContractFunction :: [Claim] -> Q [Dec]
generateContractFunction claims =
  let (fnName, behaviors) = extractBehaviors claims
      fnName' = (mkName (uncapitalise fnName ++ "Contract"))
  in generateContractDecl fnName' [("swap0", undefinedE), ("swap1", undefinedE)]
  where
    undefinedE = VarE (mkName "undefined")

-- this build the function signature for the contract, each contract generates one function
-- that recieves transactions, each contract function has it's own internal "method dispatched"
-- which matches on the method of the transaction, extracts the arguments and calls the body with
-- the arguments in scope
-- Each contract is of this shape:
-- ```
-- contractName :: ContractState -> Transaction -> ContractState
-- contractName st transaction = case method transaction of
--     "m1" -> let (arg1, arg2) <- extractm1 (args transaction) in b1
--     "m2" -> let (arg1, arg2, arg3) <- extractm2 (args transaction) in b2
--     "m3" -> let (arg1) <- extractm3 (args transaction) in b3
-- ```
generateContractDecl :: Name -> [(String, TH.Exp)] -> Q [Dec]
generateContractDecl fnName clauses = do
     let method = mkName "method"
     crashMessage <- [|error ("unexpected method, got '" ++ $(return (VarE method)) ++ "'\nexpected one of : " ++ $(return clauseLit))|]
     transactionPattern <- [p|Act.Prelude.Transaction contract $(return (VarP method)) args|]
     pure
       [ SigD
           fnName
           ((ConT (mkName "AmmState")) `arr` (ConT (mkName "Transaction") `arr` (ConT (mkName "AmmState"))))
       , FunD
           fnName
           [Clause
               [VarP (mkName "st"), transactionPattern]
               (NormalB (CaseE
                   (VarE method)
                   (matchClauses ++ [matchE WildP crashMessage])
                   ))
               []
           ]
       ]
  where
  clauseLit :: TH.Exp
  clauseLit = LitE (StringL (intercalate ", " (fmap fst clauses)))


  -- This is the list of pattern to dispatch the transaction to the correct function body for the contract
  -- ```
  -- case (method transaction) of
  --   "call1" -> body1 \
  --   "call2" -> body2  |-> This bit
  --   "call3" -> body3 /
  -- ```
  matchClauses :: [Match]
  matchClauses = fmap (\(pat, exp) -> matchE (LitP (StringL pat)) exp) clauses
  matchE p b = Match p (NormalB b) []
  undefinedE = VarE (mkName "undefined")

getUpdates4Method :: [Behaviour] -> [(String, [Rewrite])]
getUpdates4Method behaviors =
  fmap (\x -> (_name x, _stateUpdates x)) $ filter (\b -> _mode b == Pass) behaviors

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
   "contract: " ++ show (_contract b) ++ "\n" ++
   "interface: " ++ show (_interface b) ++"\n" ++
   "preconditions:\n" ++
   unlines (fmap (("  - " ++) . show) (_preconditions b)) ++
   "postConditions:\n" ++
   unlines (fmap (("  - " ++) . show) (_postconditions b)) ++
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

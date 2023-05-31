{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Act.TH (act2OG, module Act.Prelude) where

import Act.Prelude
import Act.TH.Dispatch
import Act.TH.Extractor
import Act.TH.State
import Act.Utils
import CLI
import Data.Foldable
import Data.List
import Data.Validation
import Error
import GHC.IO.Unsafe
import Language.Haskell.TH.Syntax as TH
import Syntax.Annotated

-- Convert from a an act filepath to a list of top-level declaration
-- state is converted into a record
-- invariants are converted into if-statements
-- We make one data declaration with one constructor for each method
-- which we then use for dispatching the correct functionality in the
-- contract.
-- a single top-level function is created taking in argument
-- the method dispatcher
act2OG :: String -> Q [Dec]
act2OG filename = do
  let file :: String = unsafePerformIO $ readFile filename
  let compiled :: Error String Act = compile file
  case compiled of
    Failure err ->
      reportError (extractError err)
        >> pure []
    -- A parsed Act file is a list of claims
    Success (Act store contracts) -> do
      methods <- traverse generateContractFunction contracts
      let stateTypes = stateDec4Interface store
      pure (stateTypes ++ concat methods)
  where
    extractError :: NonEmpty (Pn, String) -> String
    extractError err = ""

arr x y = AppT (AppT ArrowT x) y

-- generate a top-level function that will define the contract
-- it's type will always be the same:
-- (ContractState, ContractMethod) -> ContractState
-- This is then going to be instanciated as an open game using `fromFunctions`
generateContractFunction :: Contract -> Q [Dec]
generateContractFunction (Contract constr behav) = do
  let contractName = uncapitalise (_cname constr)
  let interface = _cinterface constr
  extractMethods <- generateExtractMethods [(_name b, _interface b) | b <- behav]
  methodClauses <- traverse mapMethod2TH behav
  generateContractDecl contractName methodClauses extractMethods

contractState :: TH.Name
contractState = mkName "contractState"

contractArgs :: TH.Name
contractArgs = mkName "args"

-- This builds the function signature for the contract, each contract generates one function
-- that recieves transactions, each contract function has it's own internal "method dispatcher"
-- that matches on the method of the transaction, extracts the arguments and calls the body with
-- the arguments in scope
-- Each contract has this shape:
--
-- ```
-- contractName :: ContractState -> Transaction -> ContractState
-- contractName st transaction = case method transaction of
--     "m1" -> let (arg1, arg2) <- extractm1 (args transaction) in b1
--     "m2" -> let (arg1, arg2, arg3) <- extractm2 (args transaction) in b2
--     "m3" -> let (arg1) <- extractm3 (args transaction) in b3
-- ```
generateContractDecl :: String -> [(String, TH.Exp)] -> [Dec] -> Q [Dec]
generateContractDecl contractName clauses extractMethods = do
  let method = mkName "method"
  crashMessage <- [|error ("unexpected method, got '" ++ $(return (VarE method)) ++ "'\\nexpected one of : " ++ $(return clauseLit))|]
  transactionPattern <- [p|Act.Prelude.Transaction _ $(return (VarP method)) $(return (VarP contractArgs))|]
  pure
    [ signatureForContract,
      FunD
        fnName
        [ Clause
            [VarP contractState, transactionPattern]
            ( NormalB
                ( CaseE
                    (VarE method)
                    (matchClauses ++ [matchE WildP crashMessage])
                )
            )
            extractMethods
        ]
    ]
  where
    -- The name of the function is the name of the contract
    fnName :: Name
    fnName = mkName (contractName ++ "Contract")

    -- The name of the type that represents the storage for the contract is given
    -- by the function `storeTypeName`. It capitalises and appends "state" to its argument
    contractStateTypeName :: Type
    contractStateTypeName = (ConT (storeTypeName contractName))

    -- The signature for a contract named "c" is given by the type
    -- c :: CState -> Transaction -> CState
    signatureForContract :: Dec
    signatureForContract =
      SigD
        fnName
        (contractStateTypeName `arr` (ConT (mkName "Transaction") `arr` contractStateTypeName))

    -- A string of all the methods, comma-separated. Used for printing errors.
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

-- Return the pattern for bringing the arguments into scope from the argument extractor
-- functions
-- If the method has no argument, the pattern is a unit
-- If the method has one argument the pattern is just a variable name
-- If the method has multiple arguments, the pattern is a tuple, one per argument
bindVariables :: [Decl] -> TH.Pat
bindVariables [] = VarP '()
bindVariables [Decl _ name] = VarP (mkName name)
bindVariables xs = TupP (fmap (VarP . mkName . getDeclId) xs)

-- A series of rewrites of the state is translated as the composition of the rewrites applied to the state
-- each rewrite is written as a single `State -> State` update function as per the implementation of `rewriteOne`
-- each constant is also written as a single `State -> State` update function
mapMethod2TH :: Behaviour -> Q (String, TH.Exp)
mapMethod2TH (Behaviour methodName _ (Interface _ args) _ _ _ actExp _) = do
  body <- RecUpdE (VarE contractState) <$> traverse rewriteOne actExp
  let extractor = AppE (VarE (argumentExtractorName methodName)) (VarE contractArgs)
  pure $ (methodName, LetE [ValD (bindVariables args) (NormalB extractor) []] body)
  where
    rewriteOne :: Rewrite -> Q (Name, TH.Exp)
    rewriteOne (Constant loc) = pure (mkName "unimplemented", VarE (mkName "undefined"))
    rewriteOne (Rewrite (Update ty (Item _ _ (SVar _ _ varName)) newValue)) =
      (mkName varName,) <$> mapExp newValue

-- The rest of this file is for debugging purposes

-- display :: String
-- display = case compiled of
--   Success s -> unlines (fmap ((++ "\n") . printClaim) s)

printBehaviour :: Behaviour -> String
printBehaviour b =
  "name: "
    ++ _name b
    ++ "\n"
    ++ "contract: "
    ++ show (_contract b)
    ++ "\n"
    ++ "interface: "
    ++ show (_interface b)
    ++ "\n"
    ++ "preconditions:\n"
    ++ unlines (fmap (("  - " ++) . show) (_preconditions b))
    ++ "postConditions:\n"
    ++ unlines (fmap (("  - " ++) . show) (_postconditions b))
    ++ "\nstate updates:\n"
    ++ unlines (fmap (("  - " ++) . show) (_stateUpdates b))

printConstructor :: Constructor -> String
printConstructor
  ( Constructor
      name
      mode
      interface
      preconditions
      postconditions
      initialStorage
      stateupdates
    ) =
    "constructor: " ++ name

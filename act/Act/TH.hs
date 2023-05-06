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
  let compiled :: Error String [Claim] =
        (compile $ file)
  case compiled of
    Failure err ->
      reportError (extractError err)
        >> pure []
    -- A parsed Act file is a list of claims
    Success val -> do
      contractFunction <- generateContractFunction val
      pure
        ( stateDec4Claim val
            ++ contractFunction
        )
  where
    extractError :: NonEmpty (Pn, String) -> String
    extractError err = ""

arr x y = AppT (AppT ArrowT x) y

-- generate a top-level function that will define the contract
-- it's type will always be the same:
-- (ContractState, ContractMethod) -> ContractState
-- This is then going to be instanciated as an open game using `fromFunctions`
generateContractFunction :: [Claim] -> Q [Dec]
generateContractFunction claims = do
  let (fnName, behaviors) = extractBehaviors claims
  let fnName' = (mkName (uncapitalise fnName ++ "Contract"))
  let methodInfo = getUpdates4Method behaviors
  methods <- traverse (\(a, b, c) -> (a,) <$> mapMethod2TH c b) methodInfo
  extractMethods <- generateExtractMethods (fmap (\(a, b, c) -> (a, c)) methodInfo)
  contractFn <- generateContractDecl fnName' methods extractMethods
  pure (contractFn)

contractState :: TH.Name
contractState = mkName "contractState"

contractArgs :: TH.Name
contractArgs = mkName "args"

-- This builds the function signature for the contract, each contract generates one function
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
generateContractDecl :: Name -> [(String, TH.Exp)] -> [Dec] -> Q [Dec]
generateContractDecl fnName clauses extractMethods = do
  let method = mkName "method"
  crashMessage <- [|error ("unexpected method, got '" ++ $(return (VarE method)) ++ "'\\nexpected one of : " ++ $(return clauseLit))|]
  transactionPattern <- [p|Act.Prelude.Transaction _ $(return (VarP method)) $(return (VarP contractArgs))|]
  pure
    [ SigD
        fnName
        ((ConT (mkName "AmmState")) `arr` (ConT (mkName "Transaction") `arr` (ConT (mkName "AmmState")))),
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

-- Extract the program associated with each method
-- Each program is a list of rewrite, the method is kept around as a string
-- The act compiler has two version of each method one for the "reverting"
-- behaviour of the method and one for the non-reverting behaviour (we know which
-- mode we're in if we check the `_mode` field and see if its `Pass`, the reverting
-- behavious is called `Fail`).
-- For Open Games we are only interested in the non-revering behaviour.
-- Transaction reversion will be handled at a later date if necessary. For now,
-- we don't have any obvious use-case for it.
getUpdates4Method :: [Behaviour] -> [(String, [Rewrite], Interface)]
getUpdates4Method behaviors =
  fmap (\x -> (_name x, _stateUpdates x, _interface x)) $ filter (\b -> _mode b == Pass) behaviors

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
mapMethod2TH :: Interface -> [Rewrite] -> Q TH.Exp
mapMethod2TH (Interface methodName args) actExp = do
  -- body <- [| ($(foldl1 (\e1 e2 -> [|$e2 . $e1|]) (fmap rewriteOne actExp))) $(return (VarE contractState)) |]
  body <- RecUpdE (VarE contractState) <$> traverse rewriteOne actExp
  let extractor = AppE (VarE (argumentExtractorName methodName)) (VarE contractArgs)
  pure $ LetE [ValD (bindVariables args) (NormalB extractor) []] body
  where
    rewriteOne :: Rewrite -> Q (Name, TH.Exp)
    rewriteOne (Constant loc) = pure (mkName "unimplemented", VarE (mkName "undefined"))
    rewriteOne (Rewrite (Update ty (Item _ nm varName _) newValue)) =
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
    ++ "mode: "
    ++ show (_mode b)
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

printClaim :: Claim -> String
printClaim (C constructor) = "constructor: " ++ show constructor
printClaim (B b) = "behaviour:\n" ++ printBehaviour b
printClaim (I i) = "invariant: " ++ show i
printClaim (S s) = "Storage: " ++ show s

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

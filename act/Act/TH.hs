{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Act.TH where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Char
import Data.Data (Data, toConstr)
import Data.FileEmbed
import Data.List
import Data.Validation
import Error

import Syntax.Annotated
import CLI

import Act.TH.Dispatch
import Act.TH.State
import Act.Utils
import Act.Prelude

import Language.Haskell.TH.Syntax as TH

getDeclId :: Decl -> String
getDeclId (Decl _ name) = name

getDeclType :: Decl -> AbiType
getDeclType (Decl ty _) = ty

deriving instance Data AbiType

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
generateContractFunction claims = do
  let (fnName, behaviors) = extractBehaviors claims
  let fnName' = (mkName (uncapitalise fnName ++ "Contract"))
  let methodInfo = getUpdates4Method behaviors
  methods <- traverse (\(a, b, c) ->  (a, ) <$> mapMethod2TH c b) methodInfo
  extractMethods <- generateExtractMethods (fmap (\(a, b, c) -> (a, c)) methodInfo)
  contractFn <- generateContractDecl fnName' methods
  pure (extractMethods ++ contractFn)

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
generateContractDecl :: Name -> [(String, TH.Exp)] -> Q [Dec]
generateContractDecl fnName clauses = do
    let method = mkName "method"
    crashMessage <- [|error ("unexpected method, got '" ++ $(return (VarE method)) ++ "'\\nexpected one of : " ++ $(return clauseLit))|]
    transactionPattern <- [p|Act.Prelude.Transaction _ $(return (VarP method)) $(return (VarP contractArgs))|]
    pure
      [ SigD
          fnName
          ((ConT (mkName "AmmState")) `arr` (ConT (mkName "Transaction") `arr` (ConT (mkName "AmmState"))))
      , FunD
          fnName
          [Clause
              [VarP contractState, transactionPattern]
              (NormalB (CaseE
                  (VarE method)
                  (matchClauses ++ [matchE WildP crashMessage])
                  ))
              []
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

-- Given each method in the contract we need to know how to extract the arguments from the
-- arguments' array. for this we create a partial top-level function which matches
-- on the argument array and return the correct number of argument in its expected type
-- as a tuple. The function is then called with the expected tuple match in order to
-- retrieve all the argument in the order expected by the body of the function
-- if a method has interface m1(a int, b uint)
-- then the extract function will look like so:
-- ```
-- extractM1 :: [AbiType] -> (Int, UInt)
-- extractM1 [AbiInt i, AbiUInt j] = (i, j)
-- extractM1 x = error ("expected arguments of type (Int, UInt) but got " ++ show x)
generateExtractMethods :: [(String, Interface)] -> Q [TH.Dec]
generateExtractMethods = fmap concat . traverse generateExtract
  where
    templateCons :: Q TH.Pat -> Q TH.Pat -> Q TH.Pat
    templateCons a b = [p| $a : $b |]

    patterns4Interface :: Interface -> Q TH.Pat
    patterns4Interface (Interface _ types) = foldr templateCons [p| [] |] $ fmap patternForDecl types

    generateExtract :: (String, Interface) -> Q [TH.Dec]
    generateExtract (name, signature) = do
      let fnName = "extract" ++ capitalise name
      sig <- extractorTypeForSignature signature
      incorrectPatternError <- [|error "unexpected arguments"|]
      patterns <- patterns4Interface signature
      pure
        [ SigD
            (mkName fnName)
            sig
        , FunD
            (mkName fnName)
            [Clause
                [patterns]
                (NormalB (expression4Interface signature))
                []
            , Clause [WildP] (NormalB (incorrectPatternError)) []]
        ]

    -- Generate a pattern for a given declaration, the declaration tells us the type of the ACT
    -- variable and therefore the constructor to use for out `AbiType` the name will be used as
    -- binding variable and used in the body to return the value of that type
    patternForDecl :: Decl -> Q TH.Pat
    patternForDecl (Decl ty name) = pure (ConP (constructorNameForType ty) [VarP (mkName name)])


    constructorNameForType :: AbiType -> Name
    constructorNameForType = mkName . show . toConstr


    -- Convert an ACT Interface into the tuple of values extracted from the list of arguments
    -- This implement the extractor function which signature is given by `extractorTypeForSignature`
    -- if the method takes no argument we return a unit value
    -- if the method takes one argument, we extract that single value from the list
    -- If the method takes multiple arguments we bundle each
    expression4Interface :: Interface -> TH.Exp
    expression4Interface (Interface _ []) = ConE '()
    expression4Interface (Interface _ [Decl _ name]) = VarE (mkName name)
    expression4Interface (Interface _ decls) = TupE (fmap (Just . VarE . mkName . getDeclId) decls)


    -- Convert a list of declarations name-type into a single tuple. If there
    -- is only one declaration then the result is the type of that declaration
    -- If there are no declarations the argument is a unit
    convertDecls :: [Decl] -> TH.Type
    convertDecls [] = ConT ''()
    convertDecls [Decl ty _] = mapAbiTypes ty
    convertDecls (Decl ty _ : decls) =
      foldl (\x y -> AppT x y) (TupleT (length decls + 1) `AppT` mapAbiTypes ty) (fmap (mapAbiTypes . getDeclType) decls)

    -- Convert an ACT interface into a haskell type
    -- This is purely for extractors so the type will always be of the shape
    --
    -- ```
    -- [AbiType] -> *argument tuple*
    -- ```
    --
    -- The argument tuple is defined by `convertDecls` the implementation of the
    -- function is handled by `expression4Interface`
    extractorTypeForSignature :: Interface -> Q Type
    extractorTypeForSignature (Interface _ decls) = [t|[AbiType] -> $(pure (convertDecls decls)) |]


extractArgument4Method :: String -> Interface -> [TH.Dec]
extractArgument4Method methodName interface = []

argumentExtractorName :: String -> TH.Name
argumentExtractorName methodName = mkName ("extract" ++ capitalise methodName)

bindVariables :: [Decl] -> TH.Pat
bindVariables [] = VarP '()
bindVariables [Decl _ name] = VarP (mkName name)
bindVariables xs = TupP (fmap (VarP . mkName . getDeclId) xs)

-- A series of rewrites of the state is translated as the composition of the rewrites applied to the state
-- each rewrite is written as a single `State -> State` update function as per the implementation of `rewriteOne`
-- each constant is also written as a single `State -> State` update function
mapMethod2TH :: Interface -> [Rewrite] -> Q TH.Exp
mapMethod2TH (Interface methodName args) actExp = do
    body <- [| ($(foldl1 (\e1 e2 -> [|$e2 . $e1|]) (fmap rewriteOne actExp))) $(return (VarE contractState)) |]
    let extractor = AppE (VarE (argumentExtractorName methodName)) (VarE contractArgs)
    pure $ LetE [ValD (bindVariables args) (NormalB extractor) []] body
    where
        rewriteOne :: Rewrite -> Q TH.Exp
        rewriteOne (Constant loc) = pure $ VarE (mkName "undefined")
        rewriteOne (Rewrite (Update ty loc newValue)) = pure $ VarE (mkName "undefined")

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

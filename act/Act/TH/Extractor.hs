{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Act.TH.Extractor (argumentExtractorName, generateExtractMethods) where

import Act.Prelude
import Act.Utils
import Data.Data
import Data.List
import Language.Haskell.TH.Syntax as TH
import Syntax.Annotated

deriving instance Data AbiType

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
-- ```
generateExtractMethods :: [(String, Interface)] -> Q [TH.Dec]
generateExtractMethods = fmap concat . traverse generateExtract

argumentExtractorName :: String -> TH.Name
argumentExtractorName methodName = mkName ("extract" ++ capitalise methodName)

transactionSignature :: Interface -> String
transactionSignature (Interface _ types) =
  let constructors = fmap (\(Decl ty _) -> show ty) types
   in concat $ intersperse ", " constructors

parens :: String -> String
parens x = "(" ++ x ++ ")"

-- Given a name and an interface, generate a function definition which extracts the arguments
-- expected from the given interface
generateExtract :: (String, Interface) -> Q [TH.Dec]
generateExtract (name, signature) = do
  let fnName = argumentExtractorName name
  sig <- extractorTypeForSignature signature
  let signatureStr = transactionSignature signature
  let asStringSplice = pure (LitE (StringL (parens signatureStr)))
  incorrectPatternError <- [|error ("unexpected arguments, got: " ++ show x ++ "\nexpected: " ++ $asStringSplice)|]
  valuePatterns <- valuePatterns4Interface signature
  pure
    [ SigD
        fnName
        sig,
      FunD
        fnName
        [ Clause
            [valuePatterns]
            (NormalB (expression4Interface signature))
            [],
          Clause [VarP (mkName "x")] (NormalB (incorrectPatternError)) []
        ]
    ]

constructorNameForType :: String -> AbiType -> Pat
constructorNameForType name (AbiUIntType _) = ConP (mkName "AbiUInt") [] [WildP, VarP (mkName name)]
constructorNameForType name (AbiIntType _) = ConP (mkName "AbiInt") [] [WildP, VarP (mkName name)]
constructorNameForType name (AbiAddressType) = ConP (mkName "AbiAddress") [] [VarP (mkName name)]
constructorNameForType name (AbiBoolType) = ConP (mkName "AbiBool") [] [VarP (mkName name)]
constructorNameForType name (AbiBytesType _) = ConP (mkName "AbiBytes") [] [WildP, VarP (mkName name)]
constructorNameForType name (AbiBytesDynamicType) = ConP (mkName "AbiBytesDynamic") [] [VarP (mkName name)]
constructorNameForType name (AbiStringType) = ConP (mkName "AbiString") [] [VarP (mkName name)]
constructorNameForType name (AbiArrayDynamicType ty) = ConP (mkName "AbiArrayDynamic") [] [WildP, VarP (mkName name)]
constructorNameForType name (AbiArrayType size ty) = ConP (mkName "AbiArray") [] [WildP, WildP, VarP (mkName name)]
constructorNameForType name (AbiTupleType types) = ConP (mkName "AbiTuple") [] [VarP (mkName name)]
constructorNameForType name (AbiFunctionType) = error "functions unsupported"

-- Generate a pattern for a given declaration, the declaration tells us the type of the ACT
-- variable and therefore the constructor to use for out `AbiType` the name will be used as
-- binding variable and used in the body to return the value of that type
patternForDecl :: Decl -> Q TH.Pat
patternForDecl (Decl ty name) = pure (constructorNameForType name ty)

templateCons :: Q TH.Pat -> Q TH.Pat -> Q TH.Pat
templateCons a b = [p|$a : $b|]

-- Generate the pattern for matching on the list of arguments of a transaction, the `Interface`
-- describes the argument tuple as a list of values and we convert it into a list-pattern
-- for each value. The type is used to create a constructor-pattern for `AbiType`
valuePatterns4Interface :: Interface -> Q TH.Pat
valuePatterns4Interface (Interface _ types) = foldr templateCons [p|[]|] $ fmap patternForDecl types

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
extractorTypeForSignature (Interface _ decls) = [t|[AbiValue] -> $(pure (convertDecls decls))|]

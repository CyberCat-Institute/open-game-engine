{-# Language TemplateHaskell #-}
{-# Language NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes  #-}

module OpenGames.Preprocessor.Compile where

import Prelude hiding (lines)
import Data.Char
import Data.Bifunctor
import OpenGames.Preprocessor.Parser
import OpenGames.Preprocessor.Lambda
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.TH
import OpenGames.Preprocessor.THSyntax
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH as TH

compileLiteral :: Literal -> Exp
compileLiteral (LInt i) = LitE $ IntegerL i
compileLiteral (LBool True) = ConE (mkName "True")
compileLiteral (LBool False) = ConE (mkName "False")
compileLiteral (LString str) = LitE $ StringL str

compileRange :: LRange -> TH.Range
compileRange (LFromR from) = FromR (compileLambda from)
compileRange (LFromThenR from step) = FromThenR (compileLambda from) (compileLambda step)
compileRange (LFromToR from to) = FromToR (compileLambda from) (compileLambda to)
compileRange (LFromThenToR from step to) = FromThenToR (compileLambda from) (compileLambda step) (compileLambda to)

compileLambda :: Lambda -> Exp
compileLambda (Lit l) = compileLiteral l
compileLambda (Var s) | isUpper (head s)  = ConE (mkName s)
                      | otherwise         = VarE (mkName s)
compileLambda (App f a) = AppE (compileLambda f) (compileLambda a)
compileLambda (Lam pat body) = LamE [compilePattern pat] (compileLambda body)
compileLambda (LList ls) = ListE $ map compileLambda ls
compileLambda (Do sm) = DoE (map toStatement sm)
  where
    toStatement :: (Maybe String, Lambda) -> Stmt
    toStatement (Nothing, lam) = NoBindS (compileLambda lam)
    toStatement (Just pat, lam) = BindS (VarP (mkName pat)) (compileLambda lam)
compileLambda (Tuple f s r) = TupE (map (compileLambda) (f : s : r))
compileLambda (Range range) = ArithSeqE (compileRange range)
compileLambda (IfThenElse prd thn els) = CondE (compileLambda prd) (compileLambda thn) (compileLambda els)
compileLambda (Ifix op left right) = InfixE (Just $ compileLambda left)
                                            (VarE $ mkName op)
                                            (Just $ compileLambda right)

compileLambda (PFix "-" arg) = AppE (VarE (mkName "negate")) (compileLambda arg)
compileLambda (PFix op arg) = error $ "unsupported prefix operator: " ++ op
compileLambda (LLet pat val body) = LetE [ValD (compilePattern pat)
                                               (NormalB (compileLambda val))
                                               []]
                                         (compileLambda body)
compileLambda (LComp stmts) = CompE (map compileStmt stmts)

compileStmt :: LStmt -> Stmt
compileStmt (LBindS pat exp) = BindS (compilePattern pat) (compileLambda exp)
compileStmt (LNoBindS exp) = NoBindS (compileLambda exp)
compileStmt (LLetS pat exp) = LetS [ValD (compilePattern pat) (NormalB $ compileLambda exp) []]

compilePattern :: Pattern -> Pat
compilePattern (PLit (LInt i)) = LitP $ IntegerL i
compilePattern (PLit (LBool True)) = ConP (mkName "True") []
compilePattern (PLit (LBool False)) = ConP (mkName "False") []
compilePattern (PLit (LString str)) = LitP $ StringL str
compilePattern (PList ls) = ListP $ fmap compilePattern ls
compilePattern (PTuple ts) = TupP $ fmap compilePattern ts
compilePattern (PVar i) = VarP (mkName i)

compLine :: Line Pattern Lambda -> Line Pat Exp
compLine = bimap compilePattern compileLambda


convertGame :: Block Pattern Lambda -> Block Pat Exp
convertGame = bimap compilePattern compileLambda

parseLambdaAsOpenGame :: String -> Maybe (FreeOpenGame Pat Exp)
parseLambdaAsOpenGame input =
  case parseLambda input of
    Left _ -> Nothing
    Right v -> Just $ compileBlock $ convertGame v

parseLambdaAsExp :: String -> Q Exp
parseLambdaAsExp input = case parseLambda input of
                           Left err -> error (show err)
                           Right v ->  (interpretOpenGame $ compileBlock $ convertGame v)

game :: QuasiQuoter
game = QuasiQuoter
     { quoteExp  = parseLambdaAsExp . dropWhile isSpace
     , quotePat  = error "expected expr"
     , quoteType = error "expected expr"
     , quoteDec  = error "expected expr"
     }

parseOrFail :: String -> Block Pattern Lambda
parseOrFail input = case parseVerbose input of
                          Left err -> error (show err)
                          Right parsed -> parsed

getParseTree = convertGame . parseOrFail

parseVerboseGame :: String -> Q Exp
parseVerboseGame = interpretOpenGame . compileBlock . getParseTree
opengame :: QuasiQuoter
opengame = QuasiQuoter
     { quoteExp  = parseVerboseGame . dropWhile isSpace
     , quotePat  = error "expected expr"
     , quoteType = error "expected expr"
     , quoteDec  = error "expected expr"
     }

parseTree :: QuasiQuoter
parseTree = QuasiQuoter
     { quoteExp  = \str -> [|getParseTree . dropWhile isSpace $ str|]
     , quotePat  = error "expected expr"
     , quoteType = error "expected expr"
     , quoteDec  = error "expected expr"
     }

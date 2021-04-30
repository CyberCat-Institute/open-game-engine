{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ArrowTest where

import GHC.Real
import Data.Bool
import Data.List
import Data.Bifunctor
import Test.Hspec as Spec
import Test.QuickCheck
import Language.Haskell.TH.Syntax as TH

import Preprocessor.Lambda
import Preprocessor.Parser
import Preprocessor.THSyntax
import Preprocessor.AbstractSyntax

import Examples.Bayesian as B

import Numeric.Probability.Distribution
import Engine.Diagnostics

btest = "t |                 <- nature (uniform [Rat, Omerta])                                -< | ;"
     ++ "x | pdMatrix1 x y   <- reindex const (decision \"prisoner1\" [Confess, DontConfess]) -< | ;"
     ++ "y | pdMatrix2 t x y <- decision \"prisoner2\" [Confess, DontConfess]                 -< | t ;"
doTest =
    "t1, t2 | <- nature (do {t1 <- uniform [BOSType1, BOSType2]; t2 <- uniform [BOSType1, BOSType2]; return (t1, t2)}) -< | ;"
 ++ "     x | bos_bayesian_matrix1 t1 x y <- decision \"man\" [BayesianB, BayesianS] -< | t1;"
 ++ "     y | bos_bayesian_matrix2 t2 x y <- decision \"woman\" [BayesianB, BayesianS] -< | t2;"

value :: GameAST String Lambda
value = MkParsedBlock [] []
  [ MkParsedLine  ["t"] [] (App (Var "nature") (App (Var "uniform") (LList [Var "Rat", Var "Omerta"]))) [] []
  , MkParsedLine  ["x"] [App (App (Var "pdMatrix1") (Var "x")) (Var "y")]
       (App (App (Var "reindex") (Var "const")) (App (App (Var "decision") (Lit $ LString "prisoner1")) (LList [Var "Confess", Var "DontConfess"])))
       [] []
  , MkParsedLine  ["y"] [App (App (App (Var "pdMatrix2") (Var "t")) (Var "x")) (Var "y")]
       (App (App (Var "decision") (Lit $ LString "prisoner2")) (LList [Var "Confess", Var "DontConfess"]))
       [] [Var "t"]
  ] [] []

simpleLine :: ParsedLine String String
simpleLine = MkParsedLine ["output"] ["input"] "middle" ["input2"] ["output2"]

simpleLam :: ParsedLine Pattern Lambda
simpleLam = undefined -- bimap undefined Var simpleLine

rangeTest =
      "t1 | <- nature (uniform [0 .. 6]) -< | ;"
   ++ "t2 | <- nature (uniform [0 .. 6]) -< | ;"
   ++ "x | playerOneUtility t1 x y <- decision \"player1\" [0 .. 12] -< | t1;"
   ++ "y | playerTwoUtility t2 x y <- decision \"player2\" [0 .. 12] -< | t2;"

simple = [Line [] [] [|nature (uniform [0..6])|] ["t1"] [],
          Line [] [] [|nature (uniform [0..6])|] ["t2"] [],
          Line [[|t1|]] [] [|decision "player1" [0..12]|] ["x"] [[|playerOneUtility t1 x y|]],
          Line [[|t2|]] [] [|decision "player2" [0..12]|] ["y"] [[|playerTwoUtility t2 x y|]]]

convertLines :: [Line p (Q Exp)] -> Q (Block p Exp)
convertLines lines = do lines <- sequence $ fmap sequence lines
                        pure (Block [] [] lines [] [])

-- main :: IO ()
-- main = do
--   hspec $ parallel $ do
--     describe "testing quasiquoted AST" $ parallel $ do
--       ref <- Spec.runIO $ TH.runQ $ (Just . THS.compileBlock <$> convertLines simple)
--       it "should parse the same freeOpenGame" $ do
--          parseLambdaAsOpenGame rangeTest
--            `shouldBe` ref
--
--       it "should be the same bayesian AST" $
--         parseLambda btest
--           `shouldBe`
--         Right value



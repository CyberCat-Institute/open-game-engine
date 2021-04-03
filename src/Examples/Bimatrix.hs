{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Examples.Bimatrix where


import Engine.BayesianGames
import Engine.OpenGames
import Preprocessor.THSyntax
import Preprocessor.AbstractSyntax
import Preprocessor.Compile

-----------------------
-- 0. Types and payoffs



data Coin = Heads | Tails deriving (Eq, Ord, Show)

matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Double
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1

---------------------
-- 1. Using TH syntax
generateGame "matchingPenniesTH" []
                        [Line [] [] [|dependentDecision "player1" (const [Heads, Tails])|] ["x"] [[|matchingPenniesMatrix1 x y|]],
                         Line [] [] [|dependentDecision "player2" (const [Heads, Tails])|] ["y"] [[|matchingPenniesMatrix2 x y|]]]


------------------------------------
-- 2. Using Quasiquotes - new parser

matchingPennies = [opengame|

    operation : dependentDecision "player1" (const [Heads, Tails]) ;
    outputs : x ;
    returns : matchingPenniesMatrix1 x y ;

    operation : dependentDecision "player1" (const [Heads, Tails]) ;
    outputs : y ;
    returns : matchingPenniesMatrix2 x y ;

|]

------------------------------------
-- 3. Using Quasiquotes - old parser
  
matchingPenniesOldQQ = [game| || =>>
  x | matchingPenniesMatrix1 x y <- dependentDecision "player1" (const [Heads, Tails]) -< | ;
  y | matchingPenniesMatrix2 x y <- dependentDecision "player2" (const [Heads, Tails]) -< | ;
  <<= ||
|]

{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric #-}
module Examples.Sequential where

import GHC.Generics
import Engine.BayesianGames
import Engine.OpenGames
import Engine.OpticClass

import Preprocessor.THSyntax
import Preprocessor.Compile
import Preprocessor.AbstractSyntax

-- Game of perfect information, considered with Bayesian equilibrium because why not
-- This example demonstrates that in pure strategies we are not detecting subgame perfect equilibria

data SequentialMove = GoLeft | GoRight deriving (Eq, Ord, Show, Generic)

sequentialMatrix1, sequentialMatrix2 :: SequentialMove -> SequentialMove -> Double
sequentialMatrix1 GoRight GoRight = 0
sequentialMatrix1 _ _ = 1
sequentialMatrix2 GoRight GoRight = 1
sequentialMatrix2 _ _ = 0

-- Using TH
generateGame "sequentialTH" [] $
                   [Line []    [] [|dependentDecision "player1" (const [GoLeft, GoRight])|] ["x"] [[|sequentialMatrix1 x y|]]
                   ,Line [[|x|]] [] [|dependentDecision "player2" (\o ->  [GoLeft, GoRight])|] ["y"] [[|sequentialMatrix2 x y|]]]

-- Using QuasiQuotes
sequential = [game| || =>>
  x | sequentialMatrix1 x y <- dependentDecision "player1" (const [GoLeft, GoRight]) -< | ;
  y | sequentialMatrix2 x y <- dependentDecision "player2" (\o -> [GoLeft, GoRight]) -< | x ;
  <<= ||
  |]

sequentialEquilibrium = evaluate sequentialTH 

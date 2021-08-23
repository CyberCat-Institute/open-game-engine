{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric #-}
module OpenGames.Examples.Sequential where

import GHC.Generics
import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.Compile
import OpenGames.Preprocessor.AbstractSyntax

-- Game of perfect information, considered with Bayesian equilibrium because why not
-- This example demonstrates that in pure strategies we are not detecting subgame perfect equilibria

data SequentialMove = GoLeft | GoRight deriving (Eq, Ord, Show, Generic)

sequentialMatrix1, sequentialMatrix2 :: SequentialMove -> SequentialMove -> Rational
sequentialMatrix1 GoRight GoRight = 0
sequentialMatrix1 _ _ = 1
sequentialMatrix2 GoRight GoRight = 1
sequentialMatrix2 _ _ = 0

-- Using TH
generateGame "sequentialTH" [] $
                   [Line (Nothing :: Maybe String) []    [] [|reindex const (decision "player1" [GoLeft, GoRight])|] ["x"] [[|sequentialMatrix1 x y|]]
                   ,Line (Nothing :: Maybe String) [param "x"] [] [|decision "player2" [GoLeft, GoRight]|] ["y"] [[|sequentialMatrix2 x y|]]]

-- Using QuasiQuotes
sequential = [game| || =>>
  x | sequentialMatrix1 x y <- reindex const (decision "player1" [GoLeft, GoRight]) -< | ;
  y | sequentialMatrix2 x y <- decision "player2" [GoLeft, GoRight] -< | x ;
  <<= ||
  |]

sequentialEquilibrium = equilibrium sequential trivialContext

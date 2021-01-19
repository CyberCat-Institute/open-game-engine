{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module OpenGames.Examples.Sequential where

import GHC.Generics
import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.THSyntax
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
                   [line []    [] [|reindex const (decision "player1" [GoLeft, GoRight])|] ["x"] [[|sequentialMatrix1 x y|]]
                   ,line [param "x"] [] [|decision "player2" [GoLeft, GoRight]|] ["y"] [[|sequentialMatrix2 x y|]]]

-- Using Blocks
sequentialSrc = Block [] []
                   [Line [] [] "reindex const (decision \"player1\" [GoLeft, GoRight])" ["x"] ["sequentialMatrix1 x y"],
                    Line ["x"] [] "decision \"player2\" [GoLeft, GoRight]" ["y"] ["sequentialMatrix2 x y"]]
                   [] []

sequential = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [GoLeft, GoRight]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), sequentialMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, x)) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [GoLeft, GoRight])))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), sequentialMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

sequentialEquilibrium = equilibrium sequential trivialContext

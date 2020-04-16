module OpenGames.Examples.Monitoring where

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Preprocessor
import OpenGames.Engine.BayesianDiagnostics
import Numeric.Probability.Distribution

monitoringGame1Src   = Block [] []
                       [Line [] [] "reindex const (decision \"upstreamFarmer\" [Crack, Flood])" ["x"] ["payoff1 x"],
                        Line [] [] "reindex const (decision \"downstreamFarmer\" [Crack, Flood])" ["y"] ["payoff2 x y"]]
                       [] []

monitoringGame1 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "upstreamFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), payoff1 x))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "downstreamFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), payoff2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

data FarmerMove = Crack | Flood deriving (Eq, Ord, Show)

payoff1 :: FarmerMove -> Rational
payoff1 Crack = 2
payoff1 Flood = 4

payoff2 :: FarmerMove -> FarmerMove -> Rational
payoff2 _ Crack = 2
payoff2 Flood Flood = 2
payoff2 Crack Flood = 4

monitoringGame1Eq = equilibrium monitoringGame1 trivialContext

{-
Example usage:

> monitoringGameEq (certainly Flood, certainly Crack)
[]
> monitoringGameEq (certainly Flood, fromFreqs [(Flood, 2), (Crack, 1)])
[]
> monitoringGameEq (uniform [Flood, Crack], fromFreqs [(Flood, 2), (Crack, 1)])
[DiagnosticInfo {player = "upstreamFarmer", observedState = "()", unobservedState = "((),())", strategy = "fromFreqs [(Crack,1 % 2),(Flood,1 % 2)]", payoff = "3 % 1", optimalMove = "Flood", optimalPayoff = "4 % 1"},DiagnosticInfo {player = "downstreamFarmer", observedState = "()", unobservedState = "((),Flood)", strategy = "fromFreqs [(Flood,2 % 3),(Crack,1 % 3)]", payoff = "8 % 3", optimalMove = "Flood", optimalPayoff = "3 % 1"},DiagnosticInfo {player = "downstreamFarmer", observedState = "()", unobservedState = "((),Crack)", strategy = "fromFreqs [(Flood,2 % 3),(Crack,1 % 3)]", payoff = "8 % 3", optimalMove = "Flood", optimalPayoff = "3 % 1"}]
-}

-- Stable institutional configuration 2

monitoringGame2Src   = Block [] []
                       [Line [] [] "reindex const (decision \"upstreamFarmer\" [Crack, Flood])" ["x"] ["payoff1 x - punishment1"],
                        Line [] [] "reindex const (decision \"downstreamFarmer\" [Crack, Flood])" ["y"] ["payoff2 x y - punishment2"]]
                       ["x", "y"] ["punishment1", "punishment2"]

monitoringGame2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y, punishment1, punishment2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y, punishment1, punishment2), ()) -> (x, y, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "upstreamFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y, punishment1, punishment2) -> ((x, y, punishment1, punishment2), payoff1 x - punishment1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y, punishment1, punishment2), ()) -> (x, y, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "downstreamFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y, punishment1, punishment2) -> ((x, y, punishment1, punishment2), payoff2 x y - punishment2))))))))) >>> (fromLens (\(x, y) -> (x, y)) (curry (\((x, y), (punishment1, punishment2)) -> (x, y, punishment1, punishment2)))))

monitoringGame3Src = Block [] []
                           [Line [] [] "monitoringGame2" ["x", "y"] ["punisher x monitorMove", "punisher y monitorMove"],
                            Line [] [] "reindex const (decision \"monitor\" [Work, Shirk])" ["monitorMove"] ["monitorPayoff monitorMove"]]
                           [] []

monitoringGame3 = reindex (\(x,y,z) -> ((x,y),z)) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y, monitorMove) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y, monitorMove), ()) -> (x, y, monitorMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((monitoringGame2)))))) >>> (fromFunctions (\((), (x, y)) -> (x, y)) (\(x, y, monitorMove) -> ((x, y, monitorMove), (punisher x monitorMove, punisher y monitorMove)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(x, y) -> ((x, y), ())) (\((x, y, monitorMove), ()) -> (x, y, monitorMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "monitor" [Work, Shirk]))))))) >>> (fromFunctions (\((x, y), monitorMove) -> (x, y, monitorMove)) (\(x, y, monitorMove) -> ((x, y, monitorMove), monitorPayoff monitorMove))))))))) >>> (fromLens (\(x, y, monitorMove) -> ()) (curry (\((x, y, monitorMove), ()) -> (x, y, monitorMove))))))

data MonitorMove = Work | Shirk deriving (Eq, Ord, Show)

monitorPayoff :: MonitorMove -> Rational
monitorPayoff Work = 1
monitorPayoff Shirk = 0

punisher :: FarmerMove -> MonitorMove -> Rational
punisher _ Shirk = 0
punisher Crack Work = 0
punisher Flood Work = 2

monitoringGame3Eq = equilibrium monitoringGame3 trivialContext

{-
Usage example

> monitoringGame3Eq (certainly Crack, certainly Crack, certainly Work)
[]
> monitoringGame3Eq (certainly Flood, certainly Flood, certainly Shirk)
[DiagnosticInfo {player = "monitor", observedState = "()", unobservedState = "((),(Flood,Flood))", strategy = "fromFreqs [(Shirk,1 % 1)]", payoff = "0 % 1", optimalMove = "Work", optimalPayoff = "1 % 1"}]
> monitoringGame3Eq (certainly Flood, certainly Flood, certainly Work)
[DiagnosticInfo {player = "downstreamFarmer", observedState = "()", unobservedState = "(((),()),Flood)", strategy = "fromFreqs [(Flood,1 % 1)]", payoff = "0 % 1", optimalMove = "Crack", optimalPayoff = "2 % 1"}]
-}

-- Stable institutional configuation 3

monitoringGame4Src = Block [] []
                           [Line [] [] "monitoringGame2" ["x", "y"] ["punisher x monitorMove", "punisher y monitorMove"],
                            Line [] [] "reindex const (decision \"monitor\" [Work, Shirk])" ["monitorMove"] ["monitorPayoff2 monitorMove x y"]]
                           [] []

monitoringGame4 = reindex (\(x,y,z) -> ((x,y),z)) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y, monitorMove) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y, monitorMove), ()) -> (x, y, monitorMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((monitoringGame2)))))) >>> (fromFunctions (\((), (x, y)) -> (x, y)) (\(x, y, monitorMove) -> ((x, y, monitorMove), (punisher x monitorMove, punisher y monitorMove)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(x, y) -> ((x, y), ())) (\((x, y, monitorMove), ()) -> (x, y, monitorMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "monitor" [Work, Shirk]))))))) >>> (fromFunctions (\((x, y), monitorMove) -> (x, y, monitorMove)) (\(x, y, monitorMove) -> ((x, y, monitorMove), monitorPayoff2 monitorMove x y))))))))) >>> (fromLens (\(x, y, monitorMove) -> ()) (curry (\((x, y, monitorMove), ()) -> (x, y, monitorMove))))))

monitorPayoff2 :: MonitorMove -> FarmerMove -> FarmerMove -> Rational
monitorPayoff2 monitorMove farmerMove1 farmerMove2 = salary - effort
  where salary = if farmerMove1 == Crack && farmerMove2 == Crack then 3 else 0
        effort = if monitorMove == Work then 1 else 0

monitoringGame4Eq = equilibrium monitoringGame4 trivialContext

{- Example usage
> monitoringGame4Eq (certainly Flood, certainly Flood, certainly Shirk)
[]
> monitoringGame4Eq (certainly Crack, certainly Crack, certainly Work)
[DiagnosticInfo {player = "monitor", observedState = "()", unobservedState = "((),(Crack,Crack))", strategy = "fromFreqs [(Work,1 % 1)]", payoff = "2 % 1", optimalMove = "Shirk", optimalPayoff = "3 % 1"}]
-}

-- Stable institutional configuration 4

monitoringGame5Src   = Block [] []
                       [Line [] [] "reindex const (decision \"upstreamFarmer\" [Crack, Flood])" ["farmerMove1"] ["payoff1 farmerMove1 - punishment1"],
                        Line [] [] "reindex const (decision \"downstreamFarmer\" [Crack, Flood])" ["farmerMove2"] ["payoff2 farmerMove1 farmerMove2 - punishment2"],
                        Line [] [] "reindex const (decision \"starvedFarmer\" [Flood])" ["farmerMove3"] ["payoff3 farmerMove1 farmerMove2 farmerMove3"]]
                       ["farmerMove1", "farmerMove2", "farmerMove3"] ["punishment1", "punishment2"]

monitoringGame5 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2), ()) -> (farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "upstreamFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\((), farmerMove1) -> farmerMove1) (\(farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2) -> ((farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2), payoff1 farmerMove1 - punishment1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\farmerMove1 -> (farmerMove1, ())) (\((farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2), ()) -> (farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "downstreamFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\(farmerMove1, farmerMove2) -> (farmerMove1, farmerMove2)) (\(farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2) -> ((farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2), payoff2 farmerMove1 farmerMove2 - punishment2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(farmerMove1, farmerMove2) -> ((farmerMove1, farmerMove2), ())) (\((farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2), ()) -> (farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "starvedFarmer" [Flood]))))))) >>> (fromFunctions (\((farmerMove1, farmerMove2), farmerMove3) -> (farmerMove1, farmerMove2, farmerMove3)) (\(farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2) -> ((farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2), payoff3 farmerMove1 farmerMove2 farmerMove3))))))))) >>> (fromLens (\(farmerMove1, farmerMove2, farmerMove3) -> (farmerMove1, farmerMove2, farmerMove3)) (curry (\((farmerMove1, farmerMove2, farmerMove3), (punishment1, punishment2)) -> (farmerMove1, farmerMove2, farmerMove3, punishment1, punishment2)))))

payoff3 :: FarmerMove -> FarmerMove -> FarmerMove -> Rational
payoff3 Flood _ _ = 0
payoff3 _ Flood _ = 0
payoff3 Crack Crack _ = 3

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
monitorPayoff Work = -1
monitorPayoff Shirk = 0

punisher :: FarmerMove -> MonitorMove -> Rational
punisher _ Shirk = 0
punisher Crack Work = 0
punisher Flood Work = 3

monitoringGame3Eq = equilibrium monitoringGame3 trivialContext

{-
Usage example

> monitoringGame3Eq (certainly Crack, certainly Crack, certainly Work)
[DiagnosticInfo {player = "monitor", observedState = "()", unobservedState = "((),(Crack,Crack))", strategy = "fromFreqs [(Work,1 % 1)]", payoff = (-1) % 1, optimalMove = "Shirk", optimalPayoff = 0 % 1}]
> monitoringGame3Eq (certainly Flood, certainly Flood, certainly Shirk)
[]
> monitoringGame3Eq (certainly Flood, certainly Flood, certainly Work)
[DiagnosticInfo {player = "upstreamFarmer", observedState = "()", unobservedState = "(((),()),())", strategy = "fromFreqs [(Flood,1 % 1)]", payoff = 1 % 1, optimalMove = "Crack", optimalPayoff = 2 % 1},DiagnosticInfo {player = "downstreamFarmer", observedState = "()", unobservedState = "(((),()),Flood)", strategy = "fromFreqs [(Flood,1 % 1)]", payoff = (-1) % 1, optimalMove = "Crack", optimalPayoff = 2 % 1},DiagnosticInfo {player = "monitor", observedState = "()", unobservedState = "((),(Flood,Flood))", strategy = "fromFreqs [(Work,1 % 1)]", payoff = (-1) % 1, optimalMove = "Shirk", optimalPayoff = 0 % 1}]
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

monitoringGame5Src = Block [] []
                            [Line [] [] "monitoringGame2" ["farmerMove1", "farmerMove2"] ["punishment1", "punishment2"],
                             Line [] [] "reindex const (decision \"starvedFarmer\" [Flood])" ["farmerMove3"] ["monitorPayoff"],
                             Line ["(farmerMove1, farmerMove2, farmerMove3)"] [] "fromFunctions (\\(x,y,z) -> payoff3 x y z) (id)" ["monitorPayoff"] []]
                            ["farmerMove1", "farmerMove2", "farmerMove3", "monitorPayoff"] ["punishment1", "punishment2"]
                            
monitoringGame5 = reindex (\(x,y,z) -> ((x,y),z,())) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2), ()) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((monitoringGame2)))))) >>> (fromFunctions (\((), (farmerMove1, farmerMove2)) -> (farmerMove1, farmerMove2)) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2) -> ((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2), (punishment1, punishment2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(farmerMove1, farmerMove2) -> ((farmerMove1, farmerMove2), ())) (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2), ()) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "starvedFarmer" [Flood]))))))) >>> (fromFunctions (\((farmerMove1, farmerMove2), farmerMove3) -> (farmerMove1, farmerMove2, farmerMove3)) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2) -> ((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2), monitorPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(farmerMove1, farmerMove2, farmerMove3) -> ((farmerMove1, farmerMove2, farmerMove3), (farmerMove1, farmerMove2, farmerMove3))) (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2), ()) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (\(x,y,z) -> payoff3 x y z) (id))))))) >>> (fromFunctions (\((farmerMove1, farmerMove2, farmerMove3), monitorPayoff) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff)) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2) -> ((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2), ()))))))))) >>> (fromLens (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff)) (curry (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff), (punishment1, punishment2)) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, punishment1, punishment2))))))

-- Nb. this made monitoring effort-free, oops

payoff3 :: FarmerMove -> FarmerMove -> FarmerMove -> Rational
payoff3 Flood _ _ = 0
payoff3 _ Flood _ = 0
payoff3 Crack Crack _ = 3

monitoringGame6Src = Block [] []
                           [Line [] [] "monitoringGame5" ["farmerMove1", "farmerMove2", "farmerMove3", "monitorPayoff"] ["punisher farmerMove1 monitorMove", "punisher farmerMove2 monitorMove"],
                            Line [] [] "reindex const (decision \"monitor\" [Work, Shirk])" ["monitorMove"] ["monitorPayoff"]]
                           [] []

monitoringGame6 = reindex (\(a,b,c,d) -> ((a,b,c),d)) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove), ()) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((monitoringGame5)))))) >>> (fromFunctions (\((), (farmerMove1, farmerMove2, farmerMove3, monitorPayoff)) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff)) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove) -> ((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove), (punisher farmerMove1 monitorMove, punisher farmerMove2 monitorMove)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff) -> ((farmerMove1, farmerMove2, farmerMove3, monitorPayoff), ())) (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove), ()) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "monitor" [Work, Shirk]))))))) >>> (fromFunctions (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff), monitorMove) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove)) (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove) -> ((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove), monitorPayoff))))))))) >>> (fromLens (\(farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove) -> ()) (curry (\((farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove), ()) -> (farmerMove1, farmerMove2, farmerMove3, monitorPayoff, monitorMove))))))

monitoringGame6Eq = equilibrium monitoringGame6 trivialContext

{- Example usage
> monitoringGame6Eq (certainly Flood, certainly Flood, certainly Flood, certainly Work)
[DiagnosticInfo {player = "upstreamFarmer", observedState = "()", unobservedState = "((((),()),()),())", strategy = "fromFreqs [(Flood,1 % 1)]", payoff = 1 % 1, optimalMove = "Crack", optimalPayoff = 2 % 1},DiagnosticInfo {player = "downstreamFarmer", observedState = "()", unobservedState = "((((),()),()),Flood)", strategy = "fromFreqs [(Flood,1 % 1)]", payoff = (-1) % 1, optimalMove = "Crack", optimalPayoff = 2 % 1}]
> monitoringGame6Eq (certainly Flood, certainly Flood, certainly Flood, certainly Shirk)
[]
> monitoringGame6Eq (certainly Crack, certainly Crack, certainly Flood, certainly Shirk)
[DiagnosticInfo {player = "upstreamFarmer", observedState = "()", unobservedState = "((((),()),()),())", strategy = "fromFreqs [(Crack,1 % 1)]", payoff = 2 % 1, optimalMove = "Flood", optimalPayoff = 4 % 1},DiagnosticInfo {player = "downstreamFarmer", observedState = "()", unobservedState = "((((),()),()),Crack)", strategy = "fromFreqs [(Crack,1 % 1)]", payoff = 2 % 1, optimalMove = "Flood", optimalPayoff = 4 % 1}]
> monitoringGame6Eq (certainly Crack, certainly Crack, certainly Flood, certainly Work)
[]
-}

-- Stable institutional configuration 5

irrigationStepSrc = Block ["startLevel"] []
                          [Line [] [] "reindex const (decision \"farmer\" [Crack, Flood])" ["farmerMove"] ["farmerWater startLevel farmerMove - punishment"]]
                          ["startLevel - farmerWater startLevel farmerMove"] ["punishment"]

irrigationStep = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(startLevel, farmerMove, punishment) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\startLevel -> (startLevel, ())) (\((startLevel, farmerMove, punishment), ()) -> (startLevel, farmerMove, punishment))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "farmer" [Crack, Flood]))))))) >>> (fromFunctions (\(startLevel, farmerMove) -> (startLevel, farmerMove)) (\(startLevel, farmerMove, punishment) -> ((startLevel, farmerMove, punishment), farmerWater startLevel farmerMove - punishment)))))))) >>> (fromLens (\(startLevel, farmerMove) -> startLevel - farmerWater startLevel farmerMove) (curry (\((startLevel, farmerMove), punishment) -> (startLevel, farmerMove, punishment)))))

farmerWater :: Rational -> FarmerMove -> Rational
farmerWater startLevel Crack = if startLevel >= 2 then 2 else startLevel
farmerWater startLevel Flood = if startLevel >= 5 then 5 else startLevel

monitoringGame7Src = Block [] []
                           [Line ["10"] [] "irrigationStep" ["levelAfter1"] ["0"],
                            Line ["levelAfter1"] [] "irrigationStep" ["levelAfter2"] ["0"],
                            Line ["levelAfter2"] [] "irrigationStep" ["levelAfter3"] ["0"]]
                           [] []


monitoringGame7 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(levelAfter1, levelAfter2, levelAfter3) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), 10)) (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\((), levelAfter1) -> levelAfter1) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\levelAfter1 -> (levelAfter1, levelAfter1)) (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\(levelAfter1, levelAfter2) -> (levelAfter1, levelAfter2)) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), 0)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(levelAfter1, levelAfter2) -> ((levelAfter1, levelAfter2), levelAfter2)) (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\((levelAfter1, levelAfter2), levelAfter3) -> (levelAfter1, levelAfter2, levelAfter3)) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), 0))))))))) >>> (fromLens (\(levelAfter1, levelAfter2, levelAfter3) -> ()) (curry (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3)))))

monitoringGame7Eq = equilibrium monitoringGame7 trivialContext

{- Example usage
> monitoringGame7Eq (certainly Flood, certainly Flood, certainly Flood)
[]
> monitoringGame7Eq (certainly Flood, certainly Flood, certainly Crack)
[]
> monitoringGame7Eq (certainly Flood, certainly Crack, certainly Flood)
[DiagnosticInfo {player = "farmer", observedState = "()", unobservedState = "(((),5 % 1),5 % 1)", strategy = "fromFreqs [(Crack,1 % 1)]", payoff = 2 % 1, optimalMove = "Flood", optimalPayoff = 5 % 1}]
> monitoringGame7Eq (certainly Crack, certainly Flood, certainly Flood)
[DiagnosticInfo {player = "farmer", observedState = "()", unobservedState = "(((),()),10 % 1)", strategy = "fromFreqs [(Crack,1 % 1)]", payoff = 2 % 1, optimalMove = "Flood", optimalPayoff = 5 % 1}]
-}

monitoringGame8Src = Block [] []
                           [Line ["10"] [] "irrigationStep" ["levelAfter1"] ["punisher2 (10 - levelAfter1) monitorMove"],
                            Line ["levelAfter1"] [] "irrigationStep" ["levelAfter2"] ["punisher2 (levelAfter1 - levelAfter2) monitorMove"],
                            Line [] [] "reindex const (decision \"lastAsFarmer\" [Crack, Flood])" ["farmerMove"] ["lastPayoff"],
                            Line [] [] "reindex const (decision \"lastAsMonitor\" [Work, Shirk])" ["monitorMove"] ["lastPayoff"],
                            Line ["levelAfter2", "farmerMove", "monitorMove"] [] "fromFunctions (\\(levelAfter2, farmerMove, monitorMove) -> farmerWater levelAfter2 farmerMove - monitorEffort monitorMove) id" ["lastPayoff"] []]
                           [] []

monitoringGame8 = reindex (\(a,b,c,d) -> (a,b,c,d,())) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), 10)) (\((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\((), levelAfter1) -> levelAfter1) (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), punisher2 (10 - levelAfter1) monitorMove))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\levelAfter1 -> (levelAfter1, levelAfter1)) (\((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\(levelAfter1, levelAfter2) -> (levelAfter1, levelAfter2)) (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), punisher2 (levelAfter1 - levelAfter2) monitorMove)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(levelAfter1, levelAfter2) -> ((levelAfter1, levelAfter2), ())) (\((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "lastAsFarmer" [Crack, Flood]))))))) >>> (fromFunctions (\((levelAfter1, levelAfter2), farmerMove) -> (levelAfter1, levelAfter2, farmerMove)) (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), lastPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(levelAfter1, levelAfter2, farmerMove) -> ((levelAfter1, levelAfter2, farmerMove), ())) (\((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "lastAsMonitor" [Work, Shirk]))))))) >>> (fromFunctions (\((levelAfter1, levelAfter2, farmerMove), monitorMove) -> (levelAfter1, levelAfter2, farmerMove, monitorMove)) (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), lastPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(levelAfter1, levelAfter2, farmerMove, monitorMove) -> ((levelAfter1, levelAfter2, farmerMove, monitorMove), (levelAfter2, farmerMove, monitorMove))) (\((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (\(levelAfter2, farmerMove, monitorMove) -> farmerWater levelAfter2 farmerMove - monitorEffort monitorMove) id)))))) >>> (fromFunctions (\((levelAfter1, levelAfter2, farmerMove, monitorMove), lastPayoff) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff)) (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()))))))))) >>> (fromLens (\(levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff) -> ()) (curry (\((levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff), ()) -> (levelAfter1, levelAfter2, farmerMove, monitorMove, lastPayoff))))))

monitorEffort :: MonitorMove -> Rational
monitorEffort Work = 1
monitorEffort Shirk = 0

punisher2 :: Rational -> MonitorMove -> Rational
punisher2 _ Shirk = 0
punisher2 x Work = if x >= 3 then 4 else 0

monitoringGame8Eq = equilibrium monitoringGame8 trivialContext

{- Example usage
> monitoringGame8Eq (certainly Crack, certainly Crack, certainly Flood, certainly Work)
[DiagnosticInfo {player = "lastAsMonitor", observedState = "()", unobservedState = "((),(8 % 1,6 % 1,Flood))", strategy = "fromFreqs [(Work,1 % 1)]", payoff = 4 % 1, optimalMove = "Shirk", optimalPayoff = 5 % 1}]
> monitoringGame8Eq (certainly Flood, certainly Flood, certainly Flood, certainly Shirk)
[]
-}
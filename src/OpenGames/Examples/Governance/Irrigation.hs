module OpenGames.Examples.Governance.Irrigation where

import           Control.Arrow (Kleisli(..))
import           Numeric.Probability.Distribution (certainly)

import           OpenGames.Examples.Governance.Monitoring (FarmerMove (..), MonitorMove (..))

import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian
import           OpenGames.Preprocessor.AbstractSyntax

farmerWater :: Double -> FarmerMove -> Double
farmerWater startLevel Crack = if startLevel >= 2 then 2 else startLevel
farmerWater startLevel Flood = if startLevel >= 5 then 5 else startLevel

irrigationStepSrc = Block ["name", "startLevel"] []
  [Line ["name", "[Crack, Flood]", "()"] [] "dependentDecision" ["farmerMove"] ["farmerWater startLevel farmerMove"]]
  ["startLevel - farmerWater startLevel farmerMove"] []

irrigationStep = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(name, startLevel, farmerMove) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel) -> ((name, startLevel), (name, [Crack, Flood], ()))) (\((name, startLevel, farmerMove), ()) -> (name, startLevel, farmerMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((name, startLevel), farmerMove) -> (name, startLevel, farmerMove)) (\(name, startLevel, farmerMove) -> ((name, startLevel, farmerMove), farmerWater startLevel farmerMove)))))))) >>> (fromLens (\(name, startLevel, farmerMove) -> startLevel - farmerWater startLevel farmerMove) (curry (\((name, startLevel, farmerMove), ()) -> (name, startLevel, farmerMove)))))

irrigationNoMonitoringSrc = Block [] []
  [Line ["\"farmer1\"", "10"] [] "irrigationStep" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1"] [] "irrigationStep" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2"] [] "irrigationStep" ["levelAfter3"] []]
  [] []

irrigationNoMonitoring = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(levelAfter1, levelAfter2, levelAfter3) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("farmer1", 10))) (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\((), levelAfter1) -> levelAfter1) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\levelAfter1 -> (levelAfter1, ("farmer2", levelAfter1))) (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\(levelAfter1, levelAfter2) -> (levelAfter1, levelAfter2)) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(levelAfter1, levelAfter2) -> ((levelAfter1, levelAfter2), ("farmer3", levelAfter2))) (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStep)))))) >>> (fromFunctions (\((levelAfter1, levelAfter2), levelAfter3) -> (levelAfter1, levelAfter2, levelAfter3)) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), ()))))))))) >>> (fromLens (\(levelAfter1, levelAfter2, levelAfter3) -> ()) (curry (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3)))))

irrigationMonitoringSrc = Block [] []
  [Line [] [] "irrigationNoMonitoring" [] [],
   Line ["\"farmer3\"", "[Work, Shirk]"] [] "dependentDecision" ["monitorDecision"] ["if monitorDecision == Work then 1 else 0"]]
  [] []

assignWater :: (Double, FarmerMove, MonitorMove) -> (Double, Double)
assignWater (startLevel, farmerMove, monitorWorks)
  = if monitorWorks == Work then ((1 - monitorPayRate)*w, monitorPayRate*w) else (w, 0)
    where w = farmerWater startLevel farmerMove

{-
first output: to farmer
second output: to monitor (*)
third output: to downstream
(*) Important subtle point: paying the monitor a fixed share of all yields if they work is
not necessarily accurate to the institution observed in field work. It is a fix
because we are modelling as a 1-shot game (partly due to limitation of the backend),
and otherwise the monitor would always have incentive to unilaterally shirk.
-}
assignWater2 :: (Double, FarmerMove, MonitorMove) -> (Double, Double, Double)
assignWater2 (startLevel, farmerMove, monitorWorks)
  = case (farmerMove, monitorWorks) of
      (Crack, Work) -> ((1 - monitorPayRate)*w, monitorPayRate*w, startLevel - w)
      (Flood, Work) -> ((1 - monitorPayRate - punishmentRate)*w, monitorPayRate*w, startLevel - (1 - punishmentRate)*w)
      (_, Shirk) -> (w, 0, startLevel - w)
  where w = farmerWater startLevel farmerMove

assignWater3 :: (Double, FarmerMove, MonitorMove) -> (Double, Double, Double)
assignWater3 (startLevel, farmerMove, monitorWorks)
  = case (farmerMove, monitorWorks) of
      (Crack, Work) -> ((1 - monitorPayRate)*w, monitorPayRate*w, startLevel - w)
      (Flood, Work) -> ((1 - monitorPayRate - punishmentRate)*w, (monitorPayRate + punishmentRate)*w, startLevel - w)
      (_, Shirk) -> (w, 0, startLevel - w)
  where w = farmerWater startLevel farmerMove

monitorPayRate :: Double
monitorPayRate = 0.499

punishmentRate :: Double
punishmentRate = 0.5

irrigationStepMonitorSrc = Block ["name", "startLevel", "monitorWorks"] ["monitorWater"]
  [Line ["name", "[Crack, Flood]", "()"] [] "dependentDecision" ["farmerMove"] ["farmerWater"],
   Line ["startLevel", "farmerMove", "monitorWorks"] ["()"] "fromFunctions assignWater id" ["farmerWater", "monitorWater"] ["()"]]
  ["startLevel - farmerWater - monitorWater"] []

irrigationStepMonitor = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, ()) -> monitorWater)) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks) -> ((name, startLevel, monitorWorks), (name, [Crack, Flood], ()))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, ()), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks), farmerMove) -> (name, startLevel, monitorWorks, farmerMove)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, ()) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, ()), farmerWater))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks, farmerMove) -> ((name, startLevel, monitorWorks, farmerMove), (startLevel, farmerMove, monitorWorks))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions assignWater id)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks, farmerMove), (farmerWater, monitorWater)) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater), ()))))))))) >>> (fromLens (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater) -> startLevel - farmerWater - monitorWater) (curry (\((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater)))))

irrigationNoMonitoring2Src = Block [] []
  [Line ["\"farmer1\"", "10", "Shirk"] ["dummy1"] "irrigationStepMonitor" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1", "Shirk"] ["dummy2"] "irrigationStepMonitor" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2", "Shirk"] ["dummy3"] "irrigationStepMonitor" ["levelAfter3"] []]
  [] []

irrigationNoMonitoring2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(levelAfter1, levelAfter2, levelAfter3, dummy3, dummy2, dummy1) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("farmer1", 10, Shirk))) (\((levelAfter1, levelAfter2, levelAfter3, dummy3, dummy2), dummy1) -> (levelAfter1, levelAfter2, levelAfter3, dummy3, dummy2, dummy1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\((), levelAfter1) -> levelAfter1) (\(levelAfter1, levelAfter2, levelAfter3, dummy3, dummy2) -> ((levelAfter1, levelAfter2, levelAfter3, dummy3, dummy2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\levelAfter1 -> (levelAfter1, ("farmer2", levelAfter1, Shirk))) (\((levelAfter1, levelAfter2, levelAfter3, dummy3), dummy2) -> (levelAfter1, levelAfter2, levelAfter3, dummy3, dummy2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\(levelAfter1, levelAfter2) -> (levelAfter1, levelAfter2)) (\(levelAfter1, levelAfter2, levelAfter3, dummy3) -> ((levelAfter1, levelAfter2, levelAfter3, dummy3), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(levelAfter1, levelAfter2) -> ((levelAfter1, levelAfter2), ("farmer3", levelAfter2, Shirk))) (\((levelAfter1, levelAfter2, levelAfter3), dummy3) -> (levelAfter1, levelAfter2, levelAfter3, dummy3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\((levelAfter1, levelAfter2), levelAfter3) -> (levelAfter1, levelAfter2, levelAfter3)) (\(levelAfter1, levelAfter2, levelAfter3) -> ((levelAfter1, levelAfter2, levelAfter3), ()))))))))) >>> (fromLens (\(levelAfter1, levelAfter2, levelAfter3) -> ()) (curry (\((levelAfter1, levelAfter2, levelAfter3), ()) -> (levelAfter1, levelAfter2, levelAfter3)))))

{- Example usage
> OpenGames.Engine.OpticClass.equilibrium irrigationNoMonitoring2 void ((a,()),(a,()),(a,()))
[DiagnosticInfo {player = "farmer1", state = "()", unobservableState = "(((),()),(\"farmer1\",10.0,False))", strategy = "fromFreqs [(Crack,1.0)]", payoff = "2.0", optimalMove = "Flood", optimalPayoff = "5.0"},DiagnosticInfo {player = "farmer2", state = "()", unobservableState = "(((),8.0),(\"farmer2\",8.0,False))", strategy = "fromFreqs [(Crack,1.0)]", payoff = "2.0", optimalMove = "Flood", optimalPayoff = "5.0"},DiagnosticInfo {player = "farmer3", state = "()", unobservableState = "(((),(8.0,6.0)),(\"farmer3\",6.0,False))", strategy = "fromFreqs [(Crack,1.0)]", payoff = "2.0", optimalMove = "Flood", optimalPayoff = "5.0"}]
> let a = Kleisli (\x -> certainly Flood)
> OpenGames.Engine.OpticClass.equilibrium irrigationNoMonitoring2 void ((a,()),(a,()),(a,()))
[]
> OpenGames.Engine.OpticClass.equilibrium irrigationNoMonitoring2 void ((a,()),(a,()),((Kleisli (\x -> certainly Crack)),()))
[]
-}

irrigationMonitor2Src = Block [] []
  [Line ["\"monitor\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorWater1 + monitorWater2 + monitorWater3 - if monitorWorks == Work then 1 else 0"],
   Line ["\"farmer1\"", "10", "monitorWorks"] ["monitorWater1"] "irrigationStepMonitor" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1", "monitorWorks"] ["monitorWater2"] "irrigationStepMonitor" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2", "monitorWorks"] ["monitorWater3"] "irrigationStepMonitor" ["levelAfter3"] []]
  [] []

irrigationMonitor2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("monitor", [Work, Shirk], ()))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1), monitorWater1 + monitorWater2 + monitorWater3 - if monitorWorks == Work then 1 else 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer1", 10, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2), monitorWater1) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\(monitorWorks, levelAfter1) -> (monitorWorks, levelAfter1)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1) -> ((monitorWorks, levelAfter1), ("farmer2", levelAfter1, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3), monitorWater2) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1), levelAfter2) -> (monitorWorks, levelAfter1, levelAfter2)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2) -> ((monitorWorks, levelAfter1, levelAfter2), ("farmer3", levelAfter2, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), monitorWater3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2), levelAfter3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()))))))))) >>> (fromLens (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ()) (curry (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)))))

irrigationTesting1Src = Block [] []
  [Line ["\"farmer\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorWater - if monitorWorks == Work then 1 else 0"],
   Line ["\"farmer\"", "10", "monitorWorks"] ["monitorWater"] "irrigationStepMonitor" ["endLevel"] []]
  [] []

irrigationTesting1 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, endLevel, monitorWater) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("farmer", [Work, Shirk], ()))) (\((monitorWorks, endLevel, monitorWater), ()) -> (monitorWorks, endLevel, monitorWater))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, endLevel, monitorWater) -> ((monitorWorks, endLevel, monitorWater), monitorWater - if monitorWorks == Work then 1 else 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer", 10, monitorWorks))) (\((monitorWorks, endLevel), monitorWater) -> (monitorWorks, endLevel, monitorWater))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\(monitorWorks, endLevel) -> (monitorWorks, endLevel)) (\(monitorWorks, endLevel) -> ((monitorWorks, endLevel), ()))))))))) >>> (fromLens (\(monitorWorks, endLevel) -> ()) (curry (\((monitorWorks, endLevel), ()) -> (monitorWorks, endLevel)))))

irrigationTesting2Src = Block [] []
  [Line ["\"monitor\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorWater - if monitorWorks == Work then 1 else 0"],
   Line ["\"farmer\"", "10", "monitorWorks"] ["monitorWater"] "irrigationStepMonitor" ["endLevel"] []]
  [] []

irrigationTesting2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, endLevel, monitorWater) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("monitor", [Work, Shirk], ()))) (\((monitorWorks, endLevel, monitorWater), ()) -> (monitorWorks, endLevel, monitorWater))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, endLevel, monitorWater) -> ((monitorWorks, endLevel, monitorWater), monitorWater - if monitorWorks == Work then 1 else 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer", 10, monitorWorks))) (\((monitorWorks, endLevel), monitorWater) -> (monitorWorks, endLevel, monitorWater))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor)))))) >>> (fromFunctions (\(monitorWorks, endLevel) -> (monitorWorks, endLevel)) (\(monitorWorks, endLevel) -> ((monitorWorks, endLevel), ()))))))))) >>> (fromLens (\(monitorWorks, endLevel) -> ()) (curry (\((monitorWorks, endLevel), ()) -> (monitorWorks, endLevel)))))

irrigationStepMonitor2Src = Block ["name", "startLevel", "monitorWorks"] ["monitorWater"]
  [Line ["name", "[Crack, Flood]", "()"] [] "dependentDecision" ["farmerMove"] ["farmerWater"],
   Line ["startLevel", "farmerMove", "monitorWorks"] ["()"] "fromFunctions assignWater2 id" ["farmerWater", "monitorWater", "downstreamWater"] ["()"]]
  ["downstreamWater"] []

irrigationStepMonitor2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater, ()) -> monitorWater)) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks) -> ((name, startLevel, monitorWorks), (name, [Crack, Flood], ()))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater, ()), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks), farmerMove) -> (name, startLevel, monitorWorks, farmerMove)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater, ()) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater, ()), farmerWater))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks, farmerMove) -> ((name, startLevel, monitorWorks, farmerMove), (startLevel, farmerMove, monitorWorks))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions assignWater2 id)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks, farmerMove), (farmerWater, monitorWater, downstreamWater)) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater), ()))))))))) >>> (fromLens (\(name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater) -> downstreamWater) (curry (\((name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, monitorWater, downstreamWater)))))

irrigationMonitor3Src = Block [] []
  [Line ["\"monitor\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorWater1 + monitorWater2 + monitorWater3 - if monitorWorks == Work then 1 else 0"],
   Line ["\"farmer1\"", "10", "monitorWorks"] ["monitorWater1"] "irrigationStepMonitor2" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1", "monitorWorks"] ["monitorWater2"] "irrigationStepMonitor2" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2", "monitorWorks"] ["monitorWater3"] "irrigationStepMonitor2" ["levelAfter3"] []]
  [] []

irrigationMonitor3 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("monitor", [Work, Shirk], ()))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1), monitorWater1 + monitorWater2 + monitorWater3 - if monitorWorks == Work then 1 else 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer1", 10, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2), monitorWater1) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2, monitorWater1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor2)))))) >>> (fromFunctions (\(monitorWorks, levelAfter1) -> (monitorWorks, levelAfter1)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1) -> ((monitorWorks, levelAfter1), ("farmer2", levelAfter1, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3), monitorWater2) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3, monitorWater2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor2)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1), levelAfter2) -> (monitorWorks, levelAfter1, levelAfter2)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2) -> ((monitorWorks, levelAfter1, levelAfter2), ("farmer3", levelAfter2, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), monitorWater3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, monitorWater3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitor2)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2), levelAfter3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()))))))))) >>> (fromLens (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ()) (curry (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)))))

irrigationMonitor3Eq a b c d = equilibrium irrigationMonitor3 void (Kleisli (const (certainly a)), (Kleisli (const (certainly b)), ()), (Kleisli (const (certainly c)), ()), (Kleisli (const (certainly d)), ()))

-- next thing: test assignWater3

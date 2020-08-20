module OpenGames.Examples.Governance.Irrigation where

import           OpenGames.Examples.Governance.Monitoring (FarmerMove (..))

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

   -- next thing to do: monitor gets paid a fraction of total yield only if they work

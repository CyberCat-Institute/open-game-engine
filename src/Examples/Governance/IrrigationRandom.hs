module OpenGames.Examples.Governance.IrrigationRandom where

import           Control.Arrow (Kleisli(..))
import           Numeric.Probability.Distribution

import           OpenGames.Examples.Governance.Monitoring (FarmerMove (..), MonitorMove (..))

import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian
import           OpenGames.Preprocessor.AbstractSyntax


monitorPayRate :: Double
monitorPayRate = 0.2

punishmentRate :: Double
punishmentRate = 0.7

farmerWater :: Double -> FarmerMove -> Double
farmerWater startLevel Crack = if startLevel >= 2 then 2 else startLevel
farmerWater startLevel Flood = if startLevel >= 5 then 5 else startLevel

monitorPayoff :: Double -> Double -> Double -> MonitorMove -> Double
monitorPayoff wage _   c Work  =  wage - c
monitorPayoff _    pun c Shirk = - pun



assignWaterNoTax :: (Double, FarmerMove, MonitorMove) -> (Double, Double)
assignWaterNoTax (startLevel, farmerMove, monitorWorks)
  = case (farmerMove, monitorWorks) of
      (Crack, Work) -> (yield, startLevel - yield)
      (Flood, Work) -> ((1 - punishmentRate)*yield, startLevel - (1 - punishmentRate)*yield)
      (_, Shirk) -> (yield, startLevel - yield)
  where yield = farmerWater startLevel farmerMove
        punishmentRate = 0.65 -- this shadows the punishment rate at file level



irrigationStepMonitorNoTaxSrc = Block ["name", "startLevel", "monitorWorks"] []
    [Line ["name", "[Crack, Flood]", "()"] [] "dependentDecision" ["farmerMove"] ["farmerWater"],
     Line ["startLevel", "farmerMove", "monitorWorks"] ["()"] "fromFunctions assignWaterNoTax id" ["farmerWater", "downstreamWater"] ["()"]]
    ["downstreamWater"] []

irrigationStepMonitorNoTax = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks) -> ((name, startLevel, monitorWorks), (name, [Crack, Flood], ()))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks), farmerMove) -> (name, startLevel, monitorWorks, farmerMove)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()), farmerWater))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks, farmerMove) -> ((name, startLevel, monitorWorks, farmerMove), (startLevel, farmerMove, monitorWorks))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions assignWaterNoTax id)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks, farmerMove), (farmerWater, downstreamWater)) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater), ()))))))))) >>> (fromLens (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater) -> downstreamWater) (curry (\((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater)))))

irrigationMonitorP1Src = Block [] []
  [Line ["\"farmer1\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorPayoff 0 0 0 monitorWorks"],
   Line ["\"farmer1\"", "9", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter3"] [],
   Line ["\"farmer1\"", "levelAfter3", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter4"] []]
  [] []

irrigationMonitorP1 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("farmer1", [Work, Shirk], ()))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), monitorPayoff 0 0 0 monitorWorks))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer1", 9, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\(monitorWorks, levelAfter1) -> (monitorWorks, levelAfter1)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1) -> ((monitorWorks, levelAfter1), ("farmer2", levelAfter1, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1), levelAfter2) -> (monitorWorks, levelAfter1, levelAfter2)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2) -> ((monitorWorks, levelAfter1, levelAfter2), ("farmer3", levelAfter2, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2), levelAfter3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ("farmer1", levelAfter3, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), levelAfter4) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()))))))))) >>> (fromLens (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ()) (curry (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4)))))


irrigationMonitorP2Src = Block [] []
  [Line ["\"farmer2\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorPayoff 0 0 0 monitorWorks"],
   Line ["\"farmer1\"", "9", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter3"] [],
   Line ["\"farmer2\"", "levelAfter3", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter4"] []]
  [] []

irrigationMonitorP2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("farmer2", [Work, Shirk], ()))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), monitorPayoff 0 0 0 monitorWorks))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer1", 9, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\(monitorWorks, levelAfter1) -> (monitorWorks, levelAfter1)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1) -> ((monitorWorks, levelAfter1), ("farmer2", levelAfter1, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1), levelAfter2) -> (monitorWorks, levelAfter1, levelAfter2)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2) -> ((monitorWorks, levelAfter1, levelAfter2), ("farmer3", levelAfter2, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2), levelAfter3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ("farmer2", levelAfter3, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), levelAfter4) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()))))))))) >>> (fromLens (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ()) (curry (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4)))))


irrigationMonitorP3Src = Block [] []
  [Line ["\"farmer3\"", "[Work, Shirk]", "()"] [] "dependentDecision" ["monitorWorks"] ["monitorPayoff 0 0 0 monitorWorks"],
   Line ["\"farmer1\"", "9", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter1"] [],
   Line ["\"farmer2\"", "levelAfter1", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter2"] [],
   Line ["\"farmer3\"", "levelAfter2", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter3"] [],
   Line ["\"farmer3\"", "levelAfter3", "monitorWorks"] [] "irrigationStepMonitorNoTax" ["levelAfter4"] []]
  [] []



irrigationMonitorP3 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ("farmer3", [Work, Shirk], ()))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((), monitorWorks) -> monitorWorks) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), monitorPayoff 0 0 0 monitorWorks))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\monitorWorks -> (monitorWorks, ("farmer1", 9, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\(monitorWorks, levelAfter1) -> (monitorWorks, levelAfter1)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1) -> ((monitorWorks, levelAfter1), ("farmer2", levelAfter1, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1), levelAfter2) -> (monitorWorks, levelAfter1, levelAfter2)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2) -> ((monitorWorks, levelAfter1, levelAfter2), ("farmer3", levelAfter2, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2), levelAfter3) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3), ("farmer3", levelAfter3, monitorWorks))) (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepMonitorNoTax)))))) >>> (fromFunctions (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3), levelAfter4) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4)) (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()))))))))) >>> (fromLens (\(monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4) -> ()) (curry (\((monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4), ()) -> (monitorWorks, levelAfter1, levelAfter2, levelAfter3, levelAfter4)))))


irrigationRandomMonitorSrc = Block [] []
  [Line [] [] "nature (uniform [Left (Left ()), Left (Right ()), Right ()])" ["switch"] [],
   Line ["switch"] [] "irrigationMonitorP1 +++ irrigationMonitorP2 +++ irrigationMonitorP3" ["discard"] []]
  [] []

irrigationRandomMonitor = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(switch, discard) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((switch, discard), ()) -> (switch, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [Left (Left ()), Left (Right ()), Right ()]))))))) >>> (fromFunctions (\((), switch) -> switch) (\(switch, discard) -> ((switch, discard), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\switch -> (switch, switch)) (\((switch, discard), ()) -> (switch, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationMonitorP1 +++ irrigationMonitorP2 +++ irrigationMonitorP3)))))) >>> (fromFunctions (\(switch, discard) -> (switch, discard)) (\(switch, discard) -> ((switch, discard), ()))))))))) >>> (fromLens (\(switch, discard) -> ()) (curry (\((switch, discard), ()) -> (switch, discard)))))


-- Testing 







rotatingStrategy1,rotatingStrategy2,rotatingStrategy3 :: (Kleisli Stochastic () MonitorMove,
                     (Kleisli Stochastic () FarmerMove, ()),
                     (Kleisli Stochastic () FarmerMove, ()),
                     (Kleisli Stochastic () FarmerMove, ()),
                     (Kleisli Stochastic () FarmerMove, ()))
rotatingStrategy1 = (Kleisli (const (certainly Work)),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()))
rotatingStrategy2 = (Kleisli (const (certainly Shirk)),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Flood)), ()))
rotatingStrategy3 = (Kleisli (const (certainly Work)),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()))


testStrategyR = OpenGames.Engine.OpticClass.equilibrium irrigationRandomMonitor void ((),((rotatingStrategy1, rotatingStrategy1),rotatingStrategy1))


testStrategyR2 = OpenGames.Engine.OpticClass.equilibrium irrigationRandomMonitor void ((),((rotatingStrategy2, rotatingStrategy2),rotatingStrategy2))

testStrategyR2' = OpenGames.Engine.OpticClass.equilibrium irrigationRandomMonitor void ((),((rotatingStrategy2, rotatingStrategy2),rotatingStrategy1))


testStrategyR3 = OpenGames.Engine.OpticClass.equilibrium irrigationRandomMonitor void ((),((rotatingStrategy2, rotatingStrategy2),rotatingStrategy3))




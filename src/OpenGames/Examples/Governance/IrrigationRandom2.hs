module OpenGames.Examples.Governance.IrrigationRandom2 where

import           Control.Arrow (Kleisli(..))
import           Data.List
import           Numeric.Probability.Distribution

import           OpenGames.Examples.Governance.Monitoring (FarmerMove (..), MonitorMove (..))

import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import           OpenGames.Engine.DependentDecision
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




irrigationStepRoleDepSrc = Block ["name" :: Agent, "startLevel", "monitorWorks"] []
  [Line ["(name,())"] [] "roleDecision [Crack, Flood]" ["farmerMove"] ["farmerWater"],
   Line ["startLevel", "farmerMove", "monitorWorks"] ["()"] "fromFunctions assignWaterNoTax id" ["farmerWater", "downstreamWater"] ["()"]]
    ["downstreamWater"] []

irrigationStepRoleDep = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks) -> ((name, startLevel, monitorWorks), (name,()))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [Crack, Flood])))))) >>> (fromFunctions (\((name, startLevel, monitorWorks), farmerMove) -> (name, startLevel, monitorWorks, farmerMove)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()), farmerWater))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(name, startLevel, monitorWorks, farmerMove) -> ((name, startLevel, monitorWorks, farmerMove), (startLevel, farmerMove, monitorWorks))) (\((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions assignWaterNoTax id)))))) >>> (fromFunctions (\((name, startLevel, monitorWorks, farmerMove), (farmerWater, downstreamWater)) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater)) (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater) -> ((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater), ()))))))))) >>> (fromLens (\(name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater) -> downstreamWater) (curry (\((name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater), ()) -> (name, startLevel, monitorWorks, farmerMove, farmerWater, downstreamWater)))))

-- TODO Change this at some point
randomOrder :: Stochastic (String,String,String)
randomOrder = do
       p1 <- uniform lst
       return (p1,((lst' p1) !! 0),(lst' p1) !! 1)
     where lst = ["farmerA","farmerB","farmerC"]
           lst' x = delete x lst

irrigationRandomRoleSrc = Block [] []
  [Line [][] "nature randomOrder" ["m", "p2", "p3"] [],
   Line ["m", "()"] [] "roleDecision [Work, Shirk]" ["monitorWorks"] ["monitorPayoff 0 0 0 monitorWorks"],
   Line ["p2", "9", "monitorWorks"] [] "irrigationStepRoleDep" ["levelAfter1"] [],
   Line ["p3", "levelAfter1", "monitorWorks"] [] "irrigationStepRoleDep" ["levelAfter2"] [],
   Line ["m", "levelAfter2", "monitorWorks"] [] "irrigationStepRoleDep" ["levelAfter3"] []]
  [] []

irrigationRandomRole = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature randomOrder)))))) >>> (fromFunctions (\((), (m,p2,p3)) -> (m,p2,p3)) (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> (((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(m,p2,p3) -> ((m,p2,p3), (m,()))) (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [Work, Shirk])))))) >>> (fromFunctions (\((m,p2,p3), monitorWorks) -> ((m,p2,p3), monitorWorks)) (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> (((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), monitorPayoff 0 0 0 monitorWorks)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((m,p2,p3), monitorWorks) -> (((m,p2,p3), monitorWorks), ("p2", 9, monitorWorks))) (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepRoleDep)))))) >>> (fromFunctions (\(((m,p2,p3), monitorWorks), levelAfter1) -> ((m,p2,p3), monitorWorks, levelAfter1)) (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> (((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((m,p2,p3), monitorWorks, levelAfter1) -> (((m,p2,p3), monitorWorks, levelAfter1), ("p3", levelAfter1, monitorWorks))) (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepRoleDep)))))) >>> (fromFunctions (\(((m,p2,p3), monitorWorks, levelAfter1), levelAfter2) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2)) (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> (((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2) -> (((m,p2,p3), monitorWorks, levelAfter1, levelAfter2), ("m", levelAfter2, monitorWorks))) (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((irrigationStepRoleDep)))))) >>> (fromFunctions (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2), levelAfter3) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3)) (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> (((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()))))))))) >>> (fromLens (\((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3) -> ()) (curry (\(((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3), ()) -> ((m,p2,p3), monitorWorks, levelAfter1, levelAfter2, levelAfter3)))))


-- Testing strategies

testStrategyRD =  OpenGames.Engine.OpticClass.equilibrium irrigationRandomRole void
                    ((),
                      Kleisli (const (certainly Work)),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()))


player1Monitor a = if a==("farmerA",()) then certainly Shirk else certainly Work


testStrategyRD2 =  OpenGames.Engine.OpticClass.equilibrium irrigationRandomRole void
                    ((),
                      Kleisli (player1Monitor),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()),
                    (Kleisli (const (certainly Crack)), ()))


testStrategyRD3 =  OpenGames.Engine.OpticClass.equilibrium irrigationRandomRole void
                    ((),
                      Kleisli (const (certainly Shirk)),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Flood)), ()),
                    (Kleisli (const (certainly Flood)), ()))

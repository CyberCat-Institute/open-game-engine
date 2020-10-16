module OpenGames.Examples.Consensus.Censor where

import Control.Arrow (Kleisli(..))
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
import OpenGames.Preprocessor.AbstractSyntax

data CensorMove = Honest | Censor deriving (Eq, Ord, Show)
data CarolObservation = Online | Offline deriving (Eq, Ord, Show)
type Deposit = Double

reward, censorPenalty, offlinePenalty :: Double
reward = 10
censorPenalty = 0.2
offlinePenalty = 0

carolObservation :: (CensorMove, CensorMove, CensorMove) -> (CarolObservation, CarolObservation, CarolObservation)
carolObservation (Honest, Honest, _) = (Online, Online, Online)
carolObservation (Honest, _, Honest) = (Online, Online, Online)
carolObservation (_, Honest, Honest) = (Online, Online, Online)
carolObservation (x, y, z) = (f x, f y, f z)
  where f Honest = Offline
        f Censor = Online

payoffs :: (Deposit, Deposit, Deposit) -> (CarolObservation, CarolObservation, CarolObservation) -> (Double, Double, Double)
payoffs (x, y, z) (Online, Online, Online) = (reward*x/(x+y+z), reward*y/(x+y+z), reward*z/(x+y+z))
payoffs (x, y, z) (Offline, Online, Online) = (-offlinePenalty*x/(x+y+z), -censorPenalty*y/(x+y+z), -censorPenalty*z/(x+y+z))
payoffs (x, y, z) (Online, Offline, Online) = (-censorPenalty*x/(x+y+z), -offlinePenalty*y/(x+y+z), -censorPenalty*z/(x+y+z))
payoffs (x, y, z) (Online, Online, Offline) = (-censorPenalty*x/(x+y+z), -censorPenalty*y/(x+y+z), -offlinePenalty*z/(x+y+z))

depositGameSrc = Block ["costOfCapital"] []
  [Line [] [] "dependentDecision \"Dave\" (const [0.0 .. 10.0])" ["daveStake"] ["-costOfCapital*daveStake"],
   Line [] [] "dependentDecision \"Erika\" (const [0.0 .. 10.0])" ["erikaStake"] ["-costOfCapital*erikaStake"],
   Line [] [] "dependentDecision \"Frank\" (const [0.0 .. 10.0])" ["frankStake"] ["-costOfCapital*frankStake"]]
  ["daveStake", "erikaStake", "frankStake"] []

depositGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\costOfCapital -> (costOfCapital, ())) (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Dave" (const [0.0 .. 10.0]))))))) >>> (fromFunctions (\(costOfCapital, daveStake) -> (costOfCapital, daveStake)) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ((costOfCapital, daveStake, erikaStake, frankStake), -costOfCapital*daveStake))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(costOfCapital, daveStake) -> ((costOfCapital, daveStake), ())) (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Erika" (const [0.0 .. 10.0]))))))) >>> (fromFunctions (\((costOfCapital, daveStake), erikaStake) -> (costOfCapital, daveStake, erikaStake)) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ((costOfCapital, daveStake, erikaStake, frankStake), -costOfCapital*erikaStake)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(costOfCapital, daveStake, erikaStake) -> ((costOfCapital, daveStake, erikaStake), ())) (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Frank" (const [0.0 .. 10.0]))))))) >>> (fromFunctions (\((costOfCapital, daveStake, erikaStake), frankStake) -> (costOfCapital, daveStake, erikaStake, frankStake)) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ((costOfCapital, daveStake, erikaStake, frankStake), -costOfCapital*frankStake))))))))) >>> (fromLens (\(costOfCapital, daveStake, erikaStake, frankStake) -> (daveStake, erikaStake, frankStake)) (curry (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake)))))

censorGameSrc = Block ["daveStake", "erikaStake", "frankStake"] []
  [Line ["daveStake"] [] "dependentDecision \"Dave\" (const [Honest, Censor])" ["daveMove"] ["davePayoff"],
   Line ["erikaStake"] [] "dependentDecision \"Erika\" (const [Honest, Censor])" ["erikaMove"] ["erikaPayoff"],
   Line ["frankStake"] [] "dependentDecision \"Frank\" (const [Honest, Censor])" ["frankMove"] ["frankPayoff"],
   Line ["(daveStake, erikaStake, frankStake)", "carolObservation (daveMove, erikaMove, frankMove)"] [] "fromFunctions (uncurry payoffs) id" ["davePayoff", "erikaPayoff", "frankPayoff"] []]
  [] []

censorGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), daveStake)) (\((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Dave" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake), daveMove) -> (daveStake, erikaStake, frankStake, daveMove)) (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), davePayoff))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, daveMove) -> ((daveStake, erikaStake, frankStake, daveMove), erikaStake)) (\((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Erika" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, daveMove), erikaMove) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove)) (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), erikaPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, daveMove, erikaMove) -> ((daveStake, erikaStake, frankStake, daveMove, erikaMove), frankStake)) (\((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Frank" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, daveMove, erikaMove), frankMove) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove)) (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), frankPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove), ((daveStake, erikaStake, frankStake), carolObservation (daveMove, erikaMove, frankMove)))) (\((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (uncurry payoffs) id)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove), (davePayoff, erikaPayoff, frankPayoff)) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff)) (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ()) (curry (\((daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff)))))

fullCensorGameSrc = Block [] []
  [Line ["0.05"] [] "depositGame" ["daveStake", "erikaStake", "frankStake"] [],
   Line ["daveStake", "erikaStake", "frankStake"] [] "censorGame" [] []]
  [] []

fullCensorGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), 0.05)) (\((daveStake, erikaStake, frankStake), ()) -> (daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((depositGame)))))) >>> (fromFunctions (\((), (daveStake, erikaStake, frankStake)) -> (daveStake, erikaStake, frankStake)) (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), (daveStake, erikaStake, frankStake))) (\((daveStake, erikaStake, frankStake), ()) -> (daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((censorGame)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake), ()) -> (daveStake, erikaStake, frankStake)) (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake) -> ()) (curry (\((daveStake, erikaStake, frankStake), ()) -> (daveStake, erikaStake, frankStake)))))

fullCensorGameEq a b c d e f = equilibrium fullCensorGame void ((Kleisli $ const a, Kleisli $ const b, Kleisli $ const c), (Kleisli d, Kleisli e, Kleisli f, ()))

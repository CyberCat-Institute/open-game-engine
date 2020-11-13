module OpenGames.Examples.Consensus.CensorStrategicAttacker where

import Control.Arrow (Kleisli(..))
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
import OpenGames.Engine.Diagnostics (player)
import OpenGames.Preprocessor.AbstractSyntax
import Numeric.Probability.Distribution (certainly, fromFreqs, uniform, T)
import Data.Ord (comparing)
import Data.List (maximumBy)
import OpenGames.Examples.Consensus.Censor

censorGame2Src = Block ["daveStake", "erikaStake", "frankStake", "bribe"] []
    [Line ["daveStake", "bribe"] [] "dependentDecision \"Dave\" (const [Honest, Censor])" ["daveMove"] ["case daveMove of {Honest -> davePayoff; Censor -> davePayoff + bribe}"],
     Line ["erikaStake"] [] "dependentDecision \"Erika\" (const [Honest, Censor])" ["erikaMove"] ["erikaPayoff"],
     Line ["frankStake"] [] "dependentDecision \"Frank\" (const [Honest, Censor])" ["frankMove"] ["frankPayoff"],
     Line ["(daveStake, erikaStake, frankStake)", "carolObservation (daveMove, erikaMove, frankMove)"] [] "fromFunctions (uncurry (payoffs censorPenalty)) id" ["davePayoff", "erikaPayoff", "frankPayoff"] []]
    ["daveMove", "erikaMove", "frankMove"] []

censorGame2 censorPenalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe) -> ((daveStake, erikaStake, frankStake, bribe), (daveStake, bribe))) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Dave" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe), daveMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), case daveMove of {Honest -> davePayoff; Censor -> davePayoff + bribe}))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove), erikaStake)) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Erika" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe, daveMove), erikaMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), erikaPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove), frankStake)) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Frank" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove), frankMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), frankPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove), ((daveStake, erikaStake, frankStake), carolObservation (daveMove, erikaMove, frankMove)))) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (uncurry (payoffs censorPenalty)) id)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove), (davePayoff, erikaPayoff, frankPayoff)) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> (daveMove, erikaMove, frankMove)) (curry (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff)))))

type AttackSuccessful = Bool

attackSuccessful :: CensorMove -> CensorMove -> CensorMove -> AttackSuccessful
attackSuccessful Censor Censor _ = True
attackSuccessful Censor _ Censor = True
attackSuccessful _ Censor Censor = True
attackSuccessful _ _ _ = False

attackerPayoff :: AttackSuccessful -> Double -> Double -> Double
attackerPayoff True amountSpent successfulAttackPayoff = successfulAttackPayoff - amountSpent
attackerPayoff False amountSpent _ = -amountSpent

attackerTypeDistribution :: T Double Double
--attackerTypeDistribution = fromFreqs [(1, 100), (10, 10), (100, 1)]
attackerTypeDistribution = fromFreqs [(0, 9), (100, 1)]
--attackerTypeDistribution = certainly 0

costOfCapital :: Double
costOfCapital = 0.1

censorBribeGame2Src = Block [] []
  [Line ["costOfCapital"] [] "depositGame" ["daveStake", "erikaStake", "frankStake"] [],
   Line [] [] "nature attackerTypeDistribution" ["successfulAttackPayoff"] [],
   Line ["daveStake", "erikaStake", "frankStake", "successfulAttackPayoff"] [] "dependentDecision \"Attacker\" (const [0, 0.1 .. 10])" ["bribe"] ["attackerPayoff (attackSuccessful daveMove erikaMove frankMove) bribe successfulAttackPayoff"],
   Line ["daveStake", "erikaStake", "frankStake", "bribe"] [] "censorGame2 censorPenalty" ["daveMove", "erikaMove", "frankMove"] []]
  [] []

censorBribeGame2 censorPenalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), costOfCapital)) (\((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((depositGame)))))) >>> (fromFunctions (\((), (daveStake, erikaStake, frankStake)) -> (daveStake, erikaStake, frankStake)) (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), ())) (\((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature attackerTypeDistribution)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake), successfulAttackPayoff) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff)) (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, successfulAttackPayoff) -> ((daveStake, erikaStake, frankStake, successfulAttackPayoff), (daveStake, erikaStake, frankStake, successfulAttackPayoff))) (\((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Attacker" (const [0, 0.1 .. 10]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, successfulAttackPayoff), bribe) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe)) (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), attackerPayoff (attackSuccessful daveMove erikaMove frankMove) bribe successfulAttackPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe) -> ((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe), (daveStake, erikaStake, frankStake, bribe))) (\((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((censorGame2 censorPenalty)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe), (daveMove, erikaMove, frankMove)) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove)) (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove) -> ()) (curry (\((daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove), ()) -> (daveStake, erikaStake, frankStake, successfulAttackPayoff, bribe, daveMove, erikaMove, frankMove)))))

attackerStrategy (_, _, _, 0) = 0
attackerStrategy (_, _, _, 1) = 0
attackerStrategy (_, _, _, 10) = 1.3
attackerStrategy (_, _, _, 30) = 1.3
attackerStrategy (_, _, _, 100) = 1.3

daveStrategy censorPenalty (_, bribe) = let davePayoff daveMove = let (x, _, _) = payoffs censorPenalty (10, 10, 10) (carolObservation (daveMove, Censor, Honest)) in x
                           in if bribe + davePayoff Censor <= davePayoff Honest then certainly Honest else certainly Censor

censorStrategy2 censorPenalty = ((Kleisli $ const $ certainly 6,
                   Kleisli $ const $ certainly 6,
                   Kleisli $ const $ certainly 6),
                   (),
                   Kleisli attackerStrategy,
                  (Kleisli (daveStrategy censorPenalty),
                   Kleisli $ const $ certainly Censor,
                   Kleisli $ const $ certainly Honest,
                   ()))

censorBribeGame2Equilibrium censorPenalty = equilibrium (censorBribeGame2 censorPenalty) void (censorStrategy2 censorPenalty)

{-
Output with
attackerStrategy (_, _, _, 0) = 0
attackerStrategy (_, _, _, 30) = 3.7
and attacker distribution
uniform [0, 30]
> censorBribeGame2Equilibrium 0.8
> mapM_ print it
DiagnosticInfo {player = "Erika", state = "10.0", unobservableState = "(((),(10.0,10.0,10.0,0.0,0.0)),(10.0,10.0,10.0,0.0,Honest))", strategy = "fromFreqs [(Censor,1.0)]", payoff = "1.5333333333333334", optimalMove = "Honest", optimalPayoff = "3.3333333333333335"}
DiagnosticInfo {player = "Erika", state = "10.0", unobservableState = "(((),(10.0,10.0,10.0,30.0,3.7)),(10.0,10.0,10.0,3.7,Censor))", strategy = "fromFreqs [(Censor,1.0)]", payoff = "1.5333333333333334", optimalMove = "Honest", optimalPayoff = "3.3333333333333335"}
-}

-- Next week: attacker can bribe multiple players
-- Some time: Generalise to a population of players

{-# LANGUAGE TemplateHaskell #-}
module OpenGames.Examples.Consensus.CensorStrategicAttacker where

import Control.Arrow (Kleisli(..))
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
import OpenGames.Engine.Diagnostics (player)
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax
import Numeric.Probability.Distribution (certainly, fromFreqs, uniform, T)
import Data.Ord (comparing)
import Data.List (maximumBy)
import OpenGames.Examples.Consensus.Censor

generateGame "censorGame2" ["censorPenalty"] $
  Block ["daveStake", "erikaStake", "frankStake", "bribe"] []
    [Line Nothing [param "daveStake", param "bribe"] []
            [|dependentDecision "Dave" (const [Honest, Censor])|]
          ["daveMove"] [[|case daveMove of {Honest -> davePayoff; Censor -> davePayoff + bribe}|]],
     Line Nothing [param "erikaStake"] [] [|dependentDecision "Erika" (const [Honest, Censor])|] ["erikaMove"] [param "erikaPayoff"],
     Line Nothing [param "frankStake"] [] [|dependentDecision "Frank" (const [Honest, Censor])|] ["frankMove"] [param "frankPayoff"],
     Line Nothing [[|(daveStake, erikaStake, frankStake)|], [|carolObservation (daveMove, erikaMove, frankMove)|]] []
            [|fromFunctions (uncurry (payoffs censorPenalty)) id|]
          ["davePayoff", "erikaPayoff", "frankPayoff"] []]
    [[|daveMove|], [|erikaMove|], [|frankMove|]] []

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

generateGame "censorBribeGame2" ["censorPenalty"] $ Block [] []
  [Line Nothing [param "costOfCapital"] [] [|depositGame|] ["daveStake", "erikaStake", "frankStake"] [],
   Line Nothing [] [] [|nature attackerTypeDistribution|] ["successfulAttackPayoff"] [],
   Line Nothing (map param ["daveStake", "erikaStake", "frankStake", "successfulAttackPayoff"]) []
          [|dependentDecision "Attacker" (const [0, 0.1 .. 10])|]
        ["bribe"] [[|attackerPayoff (attackSuccessful daveMove erikaMove frankMove) bribe successfulAttackPayoff|]],
   Line Nothing (map param ["daveStake", "erikaStake", "frankStake", "bribe"]) [] [|censorGame2 censorPenalty|] ["daveMove", "erikaMove", "frankMove"] []]
  [] []

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

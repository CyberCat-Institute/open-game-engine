{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.Consensus.AndGateMarkov where

import Numeric.Probability.Distribution (certainly, uniform, fromFreqs)

import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Compile
import OpenGames.Engine.Diagnostics
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision

import OpenGames.Examples.Consensus.DepositGame (attackerPayoff)
import OpenGames.Examples.Consensus.AndGate
import Control.Arrow (Kleisli(..))

depositStagePlayer name minDeposit maxDeposit incrementDeposit epsilon = [opengame|
  inputs : summaryStatistic, costOfCapital ;

  :---:

  inputs : summaryStatistic, costOfCapital ;
  operation : epsilonDecision epsilon name [minDeposit, minDeposit + incrementDeposit .. maxDeposit] ;
  outputs : deposit ;
  returns : (-deposit) * costOfCapital ;

  :---:

  outputs : deposit ;
|]

playingStagePlayer name moves = [opengame|
  inputs : summaryStatistic, observation, bribe ;

  :---:

  inputs : summaryStatistic, observation, bribe ;
  operation : dependentDecision name (const moves) ;
  outputs : move ;
  returns : payoff + if bribePaid then bribe else 0 ;

  :---:

  outputs : move ;
  returns : payoff, bribePaid ;
|]

andGateMarkovGame numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor = [opengame|
  inputs : summaryStatistic ;

  :---:

  inputs : replicate numPlayers (summaryStatistic, costOfCapital) ;
  feedback : discard1 ;
  operation : population (map (\n -> depositStagePlayer ("Player " ++ show n) minDeposit maxDeposit incrementDeposit epsilon) [1 .. numPlayers]) ;
  outputs : deposits ;
  returns : replicate numPlayers unit ;

  operation : nature (successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff) ;
  outputs : successfulAttackPayoff ;

  inputs : deposits, successfulAttackPayoff ;
  operation : dependentDecision "Attacker" (const [minBribe, minBribe+incrementBribe .. maxBribe]) ;
  outputs : bribe ;
  returns : attackerPayoff bribesAccepted bribe successfulAttackPayoff ;

  inputs : replicate numPlayers (summaryStatistic, deposits, bribe) ;
  feedback : discard2 ;
  operation : population (map (\n -> playingStagePlayer ("Player " ++ show n) [True, False]) [1 .. numPlayers]) ;
  outputs : moves ;
  returns : zip (payoffAndGate penalty reward deposits (obfuscateAndGate moves)) bribesAccepted ;

  inputs : moves ;
  operation : fromFunctions (map not) id ;
  outputs : bribesAccepted ;

  inputs : replicate numPlayers unit ;
  feedback : discard4 ;
  operation : population (map (\n -> discount ("Player " ++ show n) (\x -> x * discountFactor)) [1 .. numPlayers]) ;
  outputs : discard3 ;
  returns : replicate numPlayers unit ;

  operation : discount "Attacker" (\x -> x * discountFactor) ;

  :---:

  outputs : (deposits, obfuscateAndGate moves) ;
|]

iteratedAndGateMarkovGame numIterations numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor
  | (numIterations == 1) = andGateMarkovGame numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor
  | (numIterations > 1)  = reindex (\x -> (x, x)) ((andGateMarkovGame numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor)
                        >>> (iteratedAndGateMarkovGame (numIterations - 1) numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor))

foo =
  let numPlayers = 2
      reward = 1.0
      costOfCapital = 0.05
      minBribe = 0.0
      maxBribe = 0.0
      incrementBribe = 0.0
      maxSuccessfulAttackPayoff = 0.0
      attackerProbability = 0.0
      penalty = 0.5
      minDeposit = 0.0
      maxDeposit = 10.0
      incrementDeposit = 0.1
      epsilon = 0.0
      discountFactor = 0.1
      g = andGateMarkovGame numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor
          :: OpticOpenGame
                          StochasticStatefulOptic
                          StochasticStatefulContext
                          [OpenGames.Engine.Diagnostics.DiagnosticInfo]
                          ([Kleisli Stochastic (([Double], Bool), Double) Double], (),
                           Kleisli Stochastic ([Double], Double) Double,
                           [Kleisli Stochastic (([Double], Bool), [Double], Double) Bool], (), [()], ())
                          ([Double], Bool)
                          ()
                          ([Double], Bool)
                          ()
      prior = undefined
      depositStrategies = undefined
      attackerStrategy = Kleisli (const (return 0.0))
      stakingStrategies = undefined
   in equilibrium g
        (StochasticStatefulContext (do {p <- prior; return ((), p)}) (\_ _ -> return ()))
        (depositStrategies, (), attackerStrategy, stakingStrategies, (), [(),()], ())

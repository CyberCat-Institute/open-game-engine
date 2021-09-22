{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


module Examples.Staking.AndGateMarkov where

import           Engine.Engine
import           Preprocessor.Preprocessor

---------------------
-- Auxilary functions

attackerPayoff :: [Bool] -> Double -> Double -> Double
attackerPayoff bribesAccepted bribe successfulAttackPayoff
  | (numBribed == numPlayers) = successfulAttackPayoff - bribe*(fromIntegral numBribed)
  | (otherwise)               = -bribe*(fromIntegral numBribed)
  where numPlayers = length bribesAccepted
        numBribed  = length (filter id bribesAccepted)

obfuscateAndGate :: [Bool] -> Bool
obfuscateAndGate = and

successfulAttackPayoffDistribution :: Double -> Double -> Stochastic Double
successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff
  | (attackerProbability == 0) = playDeterministically 0
  | (attackerProbability == 1) = playDeterministically maxSuccessfulAttackPayoff
  | (otherwise) = distFromList [(0, 1-attackerProbability), (maxSuccessfulAttackPayoff, attackerProbability)]

payoffAndGate :: Double -> Double -> [Double] -> Bool -> [Double]
payoffAndGate penalty reward deposits True
  | (sumDeposits > 0) = [deposit*reward/sumDeposits | deposit <- deposits]
  | (sumDeposits == 0) = [0 | _ <- deposits]
  where sumDeposits = sum deposits
payoffAndGate penalty reward deposits False
  = [-penalty*deposit | deposit <- deposits]

--------
-- Games

depositStagePlayer name minDeposit maxDeposit incrementDeposit epsilon = [opengame|
  inputs : summaryStatistic, costOfCapital ;

  :---:

  inputs : summaryStatistic, costOfCapital ;
  operation : dependentEpsilonDecision epsilon name (const [minDeposit, minDeposit + incrementDeposit .. maxDeposit]) ;
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


data AndGateMarkovParams = AndGateMarkovParams {
  reward :: Double,
  costOfCapital :: Double,
  minBribe :: Double,
  maxBribe :: Double,
  incrementBribe :: Double,
  maxSuccessfulAttackPayoff :: Double,
  attackerProbability :: Double,
  penalty :: Double,
  minDeposit :: Double,
  maxDeposit :: Double,
  incrementDeposit :: Double,
  epsilon :: Double,
  discountFactor :: Double
}

stakingRound costOfCapital minDeposit maxDeposit incrementDeposit epsilon = [opengame|


  inputs : summaryStatistic ;

  :---:


  inputs : (summaryStatistic, costOfCapital) ;
  feedback : ;
  operation : depositStagePlayer "Player1" minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposit1 ;
  returns :  ;

  inputs : (summaryStatistic, costOfCapital) ;
  feedback : ;
  operation : depositStagePlayer "Player2" minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposit2 ;
  returns :  ;

  inputs : (summaryStatistic, costOfCapital) ;
  feedback : ;
  operation : depositStagePlayer "Player3" minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposit3 ;
  returns :  ;

  inputs : deposit1,deposit2,deposit3 ;
  operation : forwardFunction (\(x,y,z) -> [x,y,z]) ;
  outputs: deposits ;

  :---:

  outputs: deposits ;

 |]

indexLs = (!!)

nothing = 0.0

playingRound :: OpenGame
                          StochasticStatefulOptic
                          StochasticStatefulContext
                          ('[Kleisli Stochastic (Double, [Double], Double) Bool,
                             Kleisli Stochastic (Double, [Double], Double) Bool,
                             Kleisli Stochastic (Double, [Double], Double) Bool]
                           )
                          ('[[DiagnosticInfoBayesian (Double, [Double], Double) Bool],
                             [DiagnosticInfoBayesian (Double, [Double], Double) Bool],
                             [DiagnosticInfoBayesian (Double, [Double], Double) Bool]]
                           )
                          (Double, [Double], Double)
                          ()
                          [Bool]
                          [(Double,Bool)]
playingRound = [opengame|

  inputs : (summaryStatistic, deposits, bribe) ;

  :---:

  inputs : (summaryStatistic, deposits, bribe) ;
  feedback :  ;
  operation : playingStagePlayer "Player1" [True, False]  ;
  outputs : move1 ;
  returns : indexLs lsPayBribes 0 ;

  inputs : (summaryStatistic, deposits, bribe) ;
  feedback :  ;
  operation : playingStagePlayer "Player2" [True, False]  ;
  outputs : move2 ;
  returns : indexLs lsPayBribes 1 ;

  inputs : (summaryStatistic, deposits, bribe) ;
  feedback :  ;
  operation : playingStagePlayer "Player3" [True, False]  ;
  outputs : move3 ;
  returns : indexLs lsPayBribes 2 ;

  inputs : move1,move2,move3 ;
  operation : forwardFunction (\(x,y,z) -> [x,y,z]) ;
  outputs: moves ;

  :---:

  outputs : moves ;
  returns : lsPayBribes ;
 |]



discountingStage discountFactor = [opengame|

  inputs : ;

  :---:

  operation : discount "Player1" (\x -> x * discountFactor) ;

  operation : discount "Player2" (\x -> x * discountFactor) ;

  operation : discount "Player3" (\x -> x * discountFactor) ;

  operation : discount "Attacker" (\x -> x * discountFactor) ;

  :---:

  outputs : ;

|]

andGateMarkovGame (AndGateMarkovParams  reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor) = [opengame|

  inputs : summaryStatistic ;

  :---:

  inputs : summaryStatistic ;
  operation : stakingRound costOfCapital minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposits ;

  operation : natureDraw (successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff) ;
  outputs : successfulAttackPayoff ;

  inputs : deposits, successfulAttackPayoff ;
  operation : dependentDecision "Attacker" (const [minBribe, minBribe+incrementBribe .. maxBribe]) ;
  outputs : bribe ;
  returns : attackerPayoff bribesAccepted bribe successfulAttackPayoff ;

  inputs : (summaryStatistic, deposits, bribe) ;
  operation : playingRound ;
  outputs : moves ;
  returns : zip (payoffAndGate penalty reward deposits (obfuscateAndGate moves)) bribesAccepted ;

  inputs : moves ;
  operation : forwardFunction (map not) ;
  outputs : bribesAccepted ;

  operation : discountingStage discountFactor ;

  :---:

  outputs : (deposits, obfuscateAndGate moves) ;
|]

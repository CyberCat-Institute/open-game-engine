{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Staking.AndGateMarkov where

import Control.Arrow (Kleisli (..))
import Control.Monad.State hiding (lift, state, void)
import qualified Control.Monad.State as ST
import Debug.Trace
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- TODO change the structure of the continuation iteration
-- DONE What effect happens through the state hack and the discounting? The discounting at least does not seem to make a difference.

--------
-- Types
andGateMarkovTestParams =
  AndGateMarkovParams
    { reward = 1.0,
      costOfCapital = 0.05,
      minBribe = 0.0,
      maxBribe = 5.0,
      incrementBribe = 1.0,
      maxSuccessfulAttackPayoff = 1000.0,
      attackerProbability = 0.001,
      penalty = 0.5,
      minDeposit = 0.0,
      maxDeposit = 10.0,
      incrementDeposit = 1,
      epsilon = 0.001,
      discountFactor = 0.5
    }

data AndGateMarkovParams = AndGateMarkovParams
  { reward :: Double,
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

---------------------
-- Auxilary functions

attackerPayoff :: [Bool] -> Double -> Double -> Double
attackerPayoff bribesAccepted bribe successfulAttackPayoff
  | (numBribed == numPlayers) = successfulAttackPayoff - bribe * (fromIntegral numBribed)
  | (otherwise) = -bribe * (fromIntegral numBribed)
  where
    numPlayers = length bribesAccepted
    numBribed = length (filter id bribesAccepted)

obfuscateAndGate :: [Bool] -> Bool
obfuscateAndGate = and

successfulAttackPayoffDistribution :: Double -> Double -> Stochastic Double
successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff
  | (attackerProbability == 0) = playDeterministically 0
  | (attackerProbability == 1) = playDeterministically maxSuccessfulAttackPayoff
  | (otherwise) = distFromList [(0, 1 - attackerProbability), (maxSuccessfulAttackPayoff, attackerProbability)]

payoffAndGate :: Double -> Double -> [Double] -> Bool -> [Double]
payoffAndGate penalty reward deposits True
  | (sumDeposits > 0) = [deposit * reward / sumDeposits | deposit <- deposits]
  | (sumDeposits == 0) = [0 | _ <- deposits]
  where
    sumDeposits = sum deposits
payoffAndGate penalty reward deposits False =
  [-penalty * deposit | deposit <- deposits]

andGateTestPrior = do
  deposits <- uniformDist [replicate 3 x | x <- [0.0, 1.0 .. 10.0]]
  andResults <- uniformDist [True, False]
  return (deposits, andResults)

--------
-- Games

depositStagePlayer name minDeposit maxDeposit incrementDeposit epsilon =
  [opengame|
  inputs : summaryStatistic, costOfCapital ;

  :---:

  inputs : summaryStatistic, costOfCapital ;
  operation : dependentEpsilonDecision epsilon name (const [minDeposit, minDeposit + incrementDeposit .. maxDeposit]) ;
  outputs : deposit ;
  returns : (-deposit) * costOfCapital ;

  :---:

  outputs : deposit ;
|]

playingStagePlayer name moves =
  [opengame|
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

stakingRound costOfCapital minDeposit maxDeposit incrementDeposit epsilon =
  [opengame|


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

playingRound ::
  OpenGame
    StochasticStatefulOptic
    StochasticStatefulContext
    ( '[ Kleisli Stochastic (([Double], Bool), [Double], Double) Bool,
         Kleisli Stochastic (([Double], Bool), [Double], Double) Bool,
         Kleisli Stochastic (([Double], Bool), [Double], Double) Bool
       ]
    )
    ( '[ [DiagnosticInfoBayesian (([Double], Bool), [Double], Double) Bool],
         [DiagnosticInfoBayesian (([Double], Bool), [Double], Double) Bool],
         [DiagnosticInfoBayesian (([Double], Bool), [Double], Double) Bool]
       ]
    )
    (([Double], Bool), [Double], Double)
    ()
    [Bool]
    [(Double, Bool)]
playingRound =
  [opengame|

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

discountingStage discountFactor =
  [opengame|

  inputs : ;

  :---:

  operation : discount "Player1" (\x -> x * discountFactor) ;

  operation : discount "Player2" (\x -> x * discountFactor) ;

  operation : discount "Player3" (\x -> x * discountFactor) ;

  operation : discount "Attacker" (\x -> x * discountFactor) ;

  :---:

  outputs : ;

|]

andGateGame (AndGateMarkovParams reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor) =
  [opengame|

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

---------------
-- continuation

-- extract continuation
extractContinuation :: StochasticStatefulOptic s () s () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z, a) <- ST.lift (v x)
  trace ",,2" (pure ())
  u z ()

-- extract next state (action)
extractNextState :: StochasticStatefulOptic s () s () -> s -> Stochastic s
extractNextState (StochasticStatefulOptic v _) x = do
  (z, a) <- v x
  pure a

extractContinuation2 :: StochasticStatefulOptic Double () ([Double], Bool) () -> Double -> StateT Vector Stochastic ()
extractContinuation2 (StochasticStatefulOptic v u) x = do
  (z, a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState2 :: StochasticStatefulOptic Double () ([Double], Bool) () -> Double -> Stochastic ([Double], Bool)
extractNextState2 (StochasticStatefulOptic v _) x = do
  (z, a) <- v x
  pure a

-- Random prior indpendent of previous moves
determineContinuationPayoffs parameters 1 strat action = pure ()
determineContinuationPayoffs parameters iterator strat action = do
  trace ",,1" (pure ())
  extractContinuation executeStrat action
  nextInput <- ST.lift $ andGateTestPrior
  determineContinuationPayoffs parameters (pred iterator) strat nextInput
  where
    executeStrat = play (andGateGame parameters) strat

-- Actual moves affect next moves
determineContinuationPayoffs' parameters 1 strat action = pure ()
determineContinuationPayoffs' parameters iterator strat action = do
  trace ",,1" (pure ())
  extractContinuation executeStrat action
  nextInput <- ST.lift $ extractNextState executeStrat action
  determineContinuationPayoffs' parameters (pred iterator) strat nextInput
  where
    executeStrat = play (andGateGame parameters) strat

determineContinuationPayoffs2 iterator initialAction = do
  extractContinuation executeStrat initialAction
  nextInput <- ST.lift $ extractNextState executeStrat $ initialAction
  determineContinuationPayoffs2 (pred iterator) nextInput
  where
    executeStrat = play (andGateGame andGateMarkovTestParams) strategyTuple

determineContinuationPayoffs3 1 _ = pure ()
determineContinuationPayoffs3 iterator initialAction = do
  trace ",,2" (pure ())
  extractContinuation executeStrat initialAction
  nextInput <- ST.lift $ extractNextState executeStrat $ initialAction
  determineContinuationPayoffs3 (pred iterator) initialAction
  pure ()
  where
    executeStrat = play (andGateGame andGateMarkovTestParams) strategyTuple

-- fix context used for the evaluation
contextCont parameters iterator strat initialAction = StochasticStatefulContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffs parameters iterator strat action)

contextCont' parameters iterator strat initialAction = StochasticStatefulContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffs' parameters iterator strat action)

contextCont2 iterator initialAction = StochasticStatefulContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffs2 iterator action)

contextCont3 iterator initialAction = StochasticStatefulContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffs3 iterator action)

-----------
-- Strategy

depositStrategy = Kleisli $ \((_, previousRoundTrue), _) -> playDeterministically $ if previousRoundTrue then 4.6 else 0.0

attackerStrategy ::
  Kleisli
    Stochastic
    ([Double], Double)
    Double
attackerStrategy = Kleisli (\(deposits, successfulAttackPayoff) -> if successfulAttackPayoff == maxSuccessfulAttackPayoff andGateMarkovTestParams && sum deposits > 0.0 then playDeterministically 5.0 else playDeterministically 0.0)

signalStrategy = Kleisli $ \((_, previousRoundTrue), _, bribe) -> playDeterministically $ previousRoundTrue && bribe < 5.0

strategyTuple =
  depositStrategy
    :- depositStrategy
    :- depositStrategy
    :- attackerStrategy
    :- signalStrategy
    :- signalStrategy
    :- signalStrategy
    :- Nil

-----------------------
-- Equilibrium analysis

andGateMarkovGameEq parameters iterator strat initialAction = evaluate (andGateGame parameters) strat context
  where
    context = contextCont parameters iterator strat initialAction

eqOutput parameters iterator strat initialAction = generateIsEq $ andGateMarkovGameEq parameters iterator strat initialAction

andGateMarkovGameEq' parameters iterator strat initialAction = evaluate (andGateGame parameters) strat context
  where
    context = contextCont' parameters iterator strat initialAction

eqOutput' parameters iterator strat initialAction = generateIsEq $ andGateMarkovGameEq' parameters iterator strat initialAction

andGateMarkovGameEq2 iterator initialAction = evaluate (andGateGame andGateMarkovTestParams) strategyTuple context
  where
    context = contextCont2 iterator initialAction

eqOutput2 iterator initialAction = generateIsEq $ andGateMarkovGameEq2 iterator initialAction

andGateMarkovGameEq3 iterator initialAction = evaluate (andGateGame andGateMarkovTestParams) strategyTuple context
  where
    context = contextCont3 iterator initialAction

eqOutput3 iterator initialAction = generateIsEq $ andGateMarkovGameEq3 iterator initialAction

testInitialAction = ([10.0, 10.0, 10.0], True)

testEq iterator = eqOutput andGateMarkovTestParams iterator strategyTuple testInitialAction

testEq' iterator = eqOutput' andGateMarkovTestParams iterator strategyTuple testInitialAction

testEq2 iterator = eqOutput2 iterator testInitialAction

testEq3 iterator = eqOutput3 iterator testInitialAction

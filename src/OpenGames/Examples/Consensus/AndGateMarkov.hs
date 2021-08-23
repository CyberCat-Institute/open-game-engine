{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.Consensus.AndGateMarkov where

import Data.HashMap (Map)

import Control.Monad.State
import Numeric.Probability.Distribution (T(..), certainly, uniform, fromFreqs)

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

data AndGateMarkovParams = AndGateMarkovParams {
  numPlayers :: Int,
  reward :: Double,
  costOfCapital :: Double,
  minBribe :: Double, maxBribe :: Double, incrementBribe :: Double,
  maxSuccessfulAttackPayoff :: Double, attackerProbability :: Double,
  penalty :: Double,
  minDeposit :: Double, maxDeposit :: Double, incrementDeposit :: Double,
  epsilon :: Double,
  discountFactor :: Double
}

andGateMarkovGame (AndGateMarkovParams numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon discountFactor) = [opengame|
  inputs : summaryStatistic ;

  :---:

  label : population ;
  inputs : replicate numPlayers (summaryStatistic, costOfCapital) ;
  feedback : discard1 ;
  operation : population (map (\n -> depositStagePlayer ("Player " ++ show n) minDeposit maxDeposit incrementDeposit epsilon) [1 .. numPlayers]) ;
  outputs : deposits ;
  returns : replicate numPlayers unit ;

  label : Attack Payoff ;
  operation : nature (successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff) ;
  outputs : successfulAttackPayoff ;

  label : Attacker ;
  inputs : deposits, successfulAttackPayoff ;
  operation : dependentDecision "Attacker" (const [minBribe, minBribe+incrementBribe .. maxBribe]) ;
  outputs : bribe ;
  returns : attackerPayoff bribesAccepted bribe successfulAttackPayoff ;

  label : population2 ;
  inputs : replicate numPlayers (summaryStatistic, deposits, bribe) ;
  feedback : discard2 ;
  operation : population (map (\n -> playingStagePlayer ("Player " ++ show n) [True, False]) [1 .. numPlayers]) ;
  outputs : moves ;
  returns : zip (payoffAndGate penalty reward deposits (obfuscateAndGate moves)) bribesAccepted ;

  label : bribes ;
  inputs : moves ;
  operation : fromFunctions (map not) id ;
  outputs : bribesAccepted ;

  label : population3 ;
  inputs : replicate numPlayers unit ;
  feedback : discard4 ;
  operation : population (map (\n -> discount ("Player " ++ show n) (\x -> x * discountFactor)) [1 .. numPlayers]) ;
  outputs : discard3 ;
  returns : replicate numPlayers unit ;

  label : DiscountFactor ;
  operation : discount "Attacker" (\x -> x * discountFactor) ;

  :---:

  outputs : (deposits, obfuscateAndGate moves) ;
|]

andGateMarkovGameTree = [parseTree|
  inputs : summaryStatistic ;

  :---:

  label : population ;
  inputs : replicate numPlayers (summaryStatistic, costOfCapital) ;
  feedback : discard1 ;
  operation : population (map (\n -> depositStagePlayer ("Player " ++ show n) minDeposit maxDeposit incrementDeposit epsilon) [1 .. numPlayers]) ;
  outputs : deposits ;
  returns : replicate numPlayers unit ;

  label : Attack Payoff ;
  operation : nature (successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff) ;
  outputs : successfulAttackPayoff ;

  label : Attacker ;
  inputs : deposits, successfulAttackPayoff ;
  operation : dependentDecision "Attacker" (const [minBribe, minBribe+incrementBribe .. maxBribe]) ;
  outputs : bribe ;
  returns : attackerPayoff bribesAccepted bribe successfulAttackPayoff ;

  label : population2 ;
  inputs : replicate numPlayers (summaryStatistic, deposits, bribe) ;
  feedback : discard2 ;
  operation : population (map (\n -> playingStagePlayer ("Player " ++ show n) [True, False]) [1 .. numPlayers]) ;
  outputs : moves ;
  returns : zip (payoffAndGate penalty reward deposits (obfuscateAndGate moves)) bribesAccepted ;

  label : bribes ;
  inputs : moves ;
  operation : fromFunctions (map not) id ;
  outputs : bribesAccepted ;

  label : population3 ;
  inputs : replicate numPlayers unit ;
  feedback : discard4 ;
  operation : population (map (\n -> discount ("Player " ++ show n) (\x -> x * discountFactor)) [1 .. numPlayers]) ;
  outputs : discard3 ;
  returns : replicate numPlayers unit ;

  label : Discount Factor ;
  operation : discount "Attacker" (\x -> x * discountFactor) ;

  :---:

  outputs : (deposits, obfuscateAndGate moves) ;
|]

iteratedAndGateMarkovGame numIterations params
  | (numIterations == 1) = andGateMarkovGame params
  | (numIterations > 1)  = reindex (\x -> (x, x)) ((andGateMarkovGame params)
                        >>> (iteratedAndGateMarkovGame (numIterations - 1) params))

andGateMarkovTestParams = AndGateMarkovParams {
    numPlayers = 3,
    reward = 1.0,
    costOfCapital = 0.05,
    minBribe = 0.0,
    maxBribe = 5.0,
    incrementBribe = 1.0,
    maxSuccessfulAttackPayoff = 1000.0,
    attackerProbability = 0.001,
    penalty = 0.5,
    minDeposit = 0.0,
    maxDeposit = 10.0,
    incrementDeposit = 0.1,
    epsilon = 0.001,
    discountFactor = 0.9
}

andGateMarkovTestStrategies ::([Kleisli Stochastic (([Double], Bool), Double) Double], (),
 Kleisli Stochastic ([Double], Double) Double,
 [Kleisli Stochastic (([Double], Bool), [Double], Double) Bool], (), [()], ())
andGateMarkovTestStrategies =
  let depositStrategies = replicate (numPlayers andGateMarkovTestParams) $ Kleisli $ \((_, previousRoundTrue), _) -> certainly $ if previousRoundTrue then 4.6 else 0.0
      attackerStrategy = Kleisli (\(deposits, successfulAttackPayoff) -> if successfulAttackPayoff == maxSuccessfulAttackPayoff andGateMarkovTestParams && sum deposits > 0.0 then certainly 5.0 else certainly 0.0)
      stakingStrategies = replicate (numPlayers andGateMarkovTestParams) $ Kleisli $ \((_, previousRoundTrue), _, bribe) -> certainly $ previousRoundTrue && bribe < 5.0
   in (depositStrategies, (), attackerStrategy, stakingStrategies, (), replicate (numPlayers andGateMarkovTestParams) (), ())

andGateMarkovTestPrior = do deposits <- uniform [replicate (numPlayers andGateMarkovTestParams) x | x <- [0.0, 1.0 .. 10.0]]
                            andResults <- uniform [True, False]
                            return (deposits, andResults)

andGateMarkovStageEq
   = equilibrium (andGateMarkovGame andGateMarkovTestParams)
        (StochasticStatefulContext (do {p <- andGateMarkovTestPrior; return ((), p)}) (\_ _ -> return ()))
        andGateMarkovTestStrategies

extractContinuation :: StochasticStatefulOptic x () y () -> x -> StateT (Map String Double) (T Double) ()
extractContinuation (StochasticStatefulOptic v u) x
  = do (z, _) <- lift (v x)
       u z ()


andGateMarkovEq numIterations =
  let continuation = extractContinuation (play (iteratedAndGateMarkovGame numIterations andGateMarkovTestParams) andGateMarkovTestStrategies)
   in equilibrium (andGateMarkovGame andGateMarkovTestParams)
        (StochasticStatefulContext (do {p <- andGateMarkovTestPrior; return ((), p)})
                                   (\() -> continuation))
        andGateMarkovTestStrategies

{-
Known equilibrium: every deposits 0, plays False, attacker plays 0 in every state
-}

{-
TODO: Extreme values of discount factor aren't making sense
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Examples.Staking.AndGateMarkovMC where

import           Debug.Trace
import           OpenGames hiding (fromLens,Agent,fromFunctions,discount,nature)
import           OpenGames.Engine.IOGames
import           OpenGames.Engine.BayesianGames (uniformDist, playDeterministically)
import           OpenGames.Preprocessor

import           Control.Arrow (Kleisli(..))
import           Control.Monad.State  hiding (state, void, lift)
import qualified Control.Monad.State  as ST
import qualified Data.Vector as V
import           Numeric.Probability.Distribution hiding (map, lift, filter)
import           System.Random.MWC.CondensedTable
import           System.Random
import           System.Random.Stateful


-- TODO change the structure of the continuation iteration
-- DONE What effect happens through the state hack and the discounting? The discounting at least does not seem to make a difference.

--------
-- Types
andGateMarkovTestParams = AndGateMarkovParams {
    sampleSize1 = 100,
    sampleSize2 = 100,
    sampleSize3 = 100,
    sampleSizeA = 100,
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
    incrementDeposit = 1,
    epsilon = 0.001,
    discountFactor = 0.5
}


data AndGateMarkovParams = AndGateMarkovParams {
  sampleSize1 :: Int,
  sampleSize2 :: Int,
  sampleSize3 :: Int,
  sampleSizeA :: Int,
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


data MessageType = Bool | Double

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

successfulAttackPayoffDistribution :: Double -> Double -> CondensedTableV Double
successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff
  | (attackerProbability == 0) = tableFromProbabilities $  V.fromList [(0,0)]
  | (attackerProbability == 1) = tableFromProbabilities $  V.fromList [(maxSuccessfulAttackPayoff,1)]
  | (otherwise) = tableFromProbabilities $  V.fromList [(0, 1-attackerProbability), (maxSuccessfulAttackPayoff, attackerProbability)]

payoffAndGate :: Double -> Double -> [Double] -> Bool -> [Double]
payoffAndGate penalty reward deposits True
  | (sumDeposits > 0) = [deposit*reward/sumDeposits | deposit <- deposits]
  | (sumDeposits == 0) = [0 | _ <- deposits]
  where sumDeposits = sum deposits
payoffAndGate penalty reward deposits False
  = [-penalty*deposit | deposit <- deposits]


andGateTestPrior = do
  deposits <- uniformDist [replicate 3 x | x <- [0.0, 1.0 .. 10.0]]
  andResults <- uniformDist [True, False]
  return (deposits, andResults)

--------
-- Games

depositStagePlayer name sampleSize minDeposit maxDeposit incrementDeposit epsilon = [opengame|
  inputs : summaryStatistic, costOfCapital ;

  :---:

  inputs : summaryStatistic, costOfCapital ;
  operation : dependentDecisionIO name sampleSize [minDeposit, minDeposit + incrementDeposit .. maxDeposit] ;
  outputs : deposit ;
  returns : (-deposit) * costOfCapital ;

  :---:

  outputs : deposit ;
|]

playingStagePlayer name sampleSize moves = [opengame|
  inputs : summaryStatistic, observation, bribe ;

  :---:

  inputs : summaryStatistic, observation, bribe ;
  operation : dependentDecisionIO name sampleSize moves ;
  outputs : move ;
  returns : payoff + if bribePaid then bribe else 0 ;

  :---:

  outputs : move ;
  returns : payoff, bribePaid ;
|]


stakingRound sampleSizeP1 sampleSizeP2 sampleSizeP3 costOfCapital minDeposit maxDeposit incrementDeposit epsilon = [opengame|


  inputs : summaryStatistic ;

  :---:


  inputs : (summaryStatistic, costOfCapital) ;
  feedback : ;
  operation : depositStagePlayer "Player1" sampleSizeP1 minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposit1 ;
  returns :  ;

  inputs : (summaryStatistic, costOfCapital) ;
  feedback : ;
  operation : depositStagePlayer "Player2" sampleSizeP2 minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposit2 ;
  returns :  ;

  inputs : (summaryStatistic, costOfCapital) ;
  feedback : ;
  operation : depositStagePlayer "Player3" sampleSizeP3 minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposit3 ;
  returns :  ;

  inputs : deposit1,deposit2,deposit3 ;
  operation : fromFunctions (\(x,y,z) -> [x,y,z]) id ;
  outputs: deposits ;

  :---:

  outputs: deposits ;

 |]

indexLs = (!!)

nothing = 0.0

playingRound :: Int
             -> Int
             -> Int
             -> OpenGame
                    MonadOptic
                    MonadContext
                    ('[Kleisli CondensedTableV (([Double],Bool), [Double], Double) Bool,
                        Kleisli CondensedTableV (([Double],Bool), [Double], Double) Bool,
                        Kleisli CondensedTableV (([Double],Bool), [Double], Double) Bool]
                      )
                    ('[IO (DiagnosticsMC Bool),
                       IO (DiagnosticsMC Bool),
                       IO (DiagnosticsMC Bool)])
                    (([Double],Bool), [Double], Double)
                    ()
                    [Bool]
                    [(Double, Bool)]
playingRound sampleSizeP1 sampleSizeP2 sampleSizeP3= [opengame|

  inputs : (summaryStatistic, deposits, bribe) ;

  :---:

  inputs : (summaryStatistic, deposits, bribe) ;
  feedback :  ;
  operation : playingStagePlayer "Player1" sampleSizeP1 [True, False]  ;
  outputs : move1 ;
  returns : indexLs lsPayBribes 0 ;

  inputs : (summaryStatistic, deposits, bribe) ;
  feedback :  ;
  operation : playingStagePlayer "Player2" sampleSizeP2 [True, False]  ;
  outputs : move2 ;
  returns : indexLs lsPayBribes 1 ;

  inputs : (summaryStatistic, deposits, bribe) ;
  feedback :  ;
  operation : playingStagePlayer "Player3" sampleSizeP3 [True, False]  ;
  outputs : move3 ;
  returns : indexLs lsPayBribes 2 ;

  inputs : move1,move2,move3 ;
  operation : fromFunctions (\(x,y,z) -> [x,y,z]) id ;
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

andGateGame :: AndGateMarkovParams
                     -> OpenGame
                          MonadOptic
                          MonadContext
                          ('[Kleisli CondensedTableV (([Double], Bool), Double) Double,
                             Kleisli CondensedTableV (([Double], Bool), Double) Double,
                             Kleisli CondensedTableV (([Double], Bool), Double) Double,
                             Kleisli CondensedTableV ([Double], Double) Double,
                             Kleisli CondensedTableV (([Double], Bool), [Double], Double) Bool,
                             Kleisli CondensedTableV (([Double], Bool), [Double], Double) Bool,
                             Kleisli CondensedTableV (([Double], Bool), [Double], Double) Bool])
                          ('[IO (DiagnosticsMC Double),
                             IO (DiagnosticsMC Double),
                             IO (DiagnosticsMC Double),
                             IO (DiagnosticsMC Double),
                             IO (DiagnosticsMC Bool),
                             IO (DiagnosticsMC Bool),
                             IO (DiagnosticsMC Bool)])
                          ([Double], Bool)
                          ()
                          ([Double], Bool)
                          ()
andGateGame (AndGateMarkovParams {..} ) = [opengame|

  inputs : summaryStatistic ;

  :---:

  inputs : summaryStatistic ;
  operation : stakingRound sampleSize1 sampleSize2 sampleSize3 costOfCapital minDeposit maxDeposit incrementDeposit epsilon ;
  outputs : deposits ;

  operation : nature (successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff) ;
  outputs : successfulAttackPayoff ;

  inputs : deposits, successfulAttackPayoff ;
  operation : dependentDecisionIO "Attacker" sampleSizeA [minBribe, minBribe+incrementBribe .. maxBribe];
  outputs : bribe ;
  returns : attackerPayoff bribesAccepted bribe successfulAttackPayoff ;

  inputs : (summaryStatistic, deposits, bribe) ;
  operation : playingRound sampleSize1 sampleSize2 sampleSize3;
  outputs : moves ;
  returns : zip (payoffAndGate penalty reward deposits (obfuscateAndGate moves)) bribesAccepted ;

  inputs : moves ;
  operation : fromFunctions (map not) id ;
  outputs : bribesAccepted ;

  operation : discountingStage discountFactor ;

  :---:

  outputs : (deposits, obfuscateAndGate moves) ;
|]


---------------
-- continuation

-- extract continuation
extractContinuation :: MonadOptic s () s () -> s -> StateT Vector IO ()
extractContinuation (MonadOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: MonadOptic s () s () -> s -> IO s
extractNextState (MonadOptic v _) x = do
  (z,a) <- v x
  pure a

extractContinuation2 :: MonadOptic Double () ([Double],Bool) () -> Double -> StateT Vector IO ()
extractContinuation2 (MonadOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState2 :: MonadOptic Double () ([Double],Bool) () -> Double -> IO ([Double],Bool)
extractNextState2 (MonadOptic v _) x = do
  (z,a) <- v x
  pure a

-- Random prior indpendent of previous moves
determineContinuationPayoffs parameters 1        strat action = pure ()
determineContinuationPayoffs parameters iterator strat action = do
   extractContinuation executeStrat action
   g <- newStdGen
   gS <- newIOGenM g
   nextInput <- ST.lift $ genFromTable (transformStochasticToProbTable andGateTestPrior) gS
   determineContinuationPayoffs parameters (pred iterator) strat nextInput
 where executeStrat =  play (andGateGame parameters) strat

-- Actual moves affect next moves
determineContinuationPayoffs' parameters 1        strat action = pure ()
determineContinuationPayoffs' parameters iterator strat action = do
   extractContinuation executeStrat action
   nextInput <- ST.lift $ extractNextState executeStrat action
   determineContinuationPayoffs' parameters (pred iterator) strat nextInput
 where executeStrat =  play (andGateGame parameters) strat

determineContinuationPayoffs3 1        _             = pure ()
determineContinuationPayoffs3 iterator initialAction = do
  extractContinuation executeStrat initialAction
  nextInput <- ST.lift $ extractNextState executeStrat $ initialAction
  determineContinuationPayoffs3 (pred iterator) initialAction
  pure ()
 where executeStrat =  play (andGateGame andGateMarkovTestParams) strategyTuple

-- fix context used for the evaluation
contextCont parameters iterator strat initialAction = MonadContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs parameters iterator strat action)

contextCont' parameters iterator strat initialAction = MonadContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs' parameters iterator strat action)

contextCont3 iterator initialAction = MonadContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs3 iterator action)




-----------
-- Strategy

transformStrat :: Kleisli Stochastic x y -> Kleisli CondensedTableV x y
transformStrat strat = Kleisli (\x ->
  let y = runKleisli strat x
      ls = decons y
      v = V.fromList ls
      in tableFromProbabilities v)

transformStochasticToProbTable dist =
  let ls = decons dist
      v  = V.fromList ls
      in tableFromProbabilities v


depositStrategy = transformStrat $  Kleisli $ \((_, previousRoundTrue), _) -> playDeterministically $ if previousRoundTrue then 4.6 else 0.0

attackerStrategy = transformStrat $ Kleisli (\(deposits, successfulAttackPayoff) -> if successfulAttackPayoff == maxSuccessfulAttackPayoff andGateMarkovTestParams && sum deposits > 0.0 then playDeterministically 5.0 else playDeterministically 0.0)

signalStrategy = transformStrat $ Kleisli $ \((_, previousRoundTrue), _, bribe) -> playDeterministically $ previousRoundTrue && bribe < 5.0


strategyTuple =
      depositStrategy
  ::- depositStrategy
  ::- depositStrategy
  ::- attackerStrategy
  ::- signalStrategy
  ::- signalStrategy
  ::- signalStrategy
  ::- Nil


-----------------------
-- Equilibrium analysis


andGateMarkovGameEq parameters iterator strat initialAction = evaluate (andGateGame parameters) strat context
  where context  = contextCont parameters iterator strat initialAction

eqOutput parameters iterator strat initialAction = andGateMarkovGameEq parameters iterator strat initialAction



andGateMarkovGameEq' parameters iterator strat initialAction = evaluate (andGateGame parameters) strat context
  where context  = contextCont' parameters iterator strat initialAction

eqOutput' parameters iterator strat initialAction =  andGateMarkovGameEq' parameters iterator strat initialAction


andGateMarkovGameEq3 iterator initialAction = evaluate (andGateGame andGateMarkovTestParams) strategyTuple context
  where context  = contextCont3 iterator initialAction

eqOutput3 iterator initialAction =  andGateMarkovGameEq3 iterator initialAction



testInitialAction = ([10.0,10.0,10.0],True)

testEq iterator = eqOutput andGateMarkovTestParams iterator strategyTuple testInitialAction

testEq' iterator = eqOutput' andGateMarkovTestParams iterator strategyTuple testInitialAction

testEq3 iterator = eqOutput3 iterator testInitialAction

printOutput :: List
                '[IO (DiagnosticsMC Double), IO (DiagnosticsMC Double),
                  IO (DiagnosticsMC Double), IO (DiagnosticsMC Double),
                  IO (DiagnosticsMC Bool), IO (DiagnosticsMC Bool),
                  IO (DiagnosticsMC Bool)]
             -> IO ()
printOutput (result1 ::- result2 ::- result3 ::- result4 ::- result5 ::- result6 ::- result7 ::- Nil) = do
  result1' <- result1
  result2' <- result2
  result3' <- result3
  result4' <- result4
  result5' <- result5
  result6' <- result6
  result7' <- result7
  putStrLn "Player1"
  print result1'
  putStrLn "Player2"
  print result2'
  putStrLn "Player3"
  print result3'
  putStrLn "Player4"
  print result4'
  putStrLn "Player5"
  print result5'
  putStrLn "Player6"
  print result6'
  putStrLn "Player7"
  print result7'








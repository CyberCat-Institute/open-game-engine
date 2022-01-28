{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Examples.Consensus.AndGate where

import Numeric.Probability.Distribution (certainly, uniform, fromFreqs)

import OpenGames.Preprocessor.CompileBlock
import OpenGames.Engine.OpticClass
import OpenGames.Engine.IOGames
import OpenGames.Engine.OpenGames

-- import OpenGames.Engine.OpenGamesClass
-- import OpenGames.Engine.OpticClass
-- import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)
-- import OpenGames.Engine.DependentDecision

import OpenGames.Examples.Consensus.DepositGame (depositStagePlayerTH, playingStagePlayer, attackerPayoff)
-- import Control.Arrow (Kleisli(..))

obfuscateAndGate :: [Bool] -> Bool
obfuscateAndGate = and

sample = 1

payoffAndGate :: Double -> Double -> [Double] -> Bool -> [Double]
payoffAndGate penalty reward deposits True
  | (sumDeposits > 0) = [deposit*reward/sumDeposits | deposit <- deposits]
  | (sumDeposits == 0) = [0 | _ <- deposits]
  where sumDeposits = sum deposits
payoffAndGate penalty reward deposits False
  = [-penalty*deposit | deposit <- deposits]

unit = ()

-- population :: Natural n -> Vec n (OpenGame o c '[p] '[q] a b s t)
--           -> OpenGame o c p :- p :- p ... n times
--                           (n copies q)
--                           Vect n a
--                           Vect n b
--                           Vect n s
--                           Vect n t
population = undefined
nature = undefined

successfulAttackPayoffDistribution :: Double -> Double -> IO Double
successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff
  | (attackerProbability == 0) = pure 0
  | (attackerProbability == 1) = pure maxSuccessfulAttackPayoff
  | (otherwise) = undefined -- do some randomIO -- fromFreqs [(0, 1-attackerProbability), (maxSuccessfulAttackPayoff, attackerProbability)]

andGateGame
  numPlayers reward costOfCapital
  minBribe maxBribe incrementBribe
  maxSuccessfulAttackPayoff attackerProbability penalty
  minDeposit maxDeposit incrementDeposit epsilon =
  [opengame|
  inputs : replicate numPlayers costOfCapital ;
  feedback : discard1 ;
  operation : population [ depositStagePlayerTH ("Player " ++ show n) minDeposit maxDeposit incrementDeposit | n <- [1 .. numPlayers]] ;
  outputs : deposits ;
  returns : replicate numPlayers unit ;

  operation : nature (successfulAttackPayoffDistribution attackerProbability maxSuccessfulAttackPayoff) ;
  outputs : successfulAttackPayoff ;

  inputs : deposits, successfulAttackPayoff ;
  operation : dependentDecisionIO "Attacker" sample ([minBribe, minBribe+incrementBribe .. maxBribe]) ;
  outputs : bribe ;
  returns : attackerPayoff bribesAccepted bribe successfulAttackPayoff ;

  inputs : replicate numPlayers (deposits, bribe) ;
  feedback : discard2 ;
  operation : population (map (\n -> playingStagePlayer ("Player " ++ show n) [True, False]) [1 .. numPlayers]) ;
  outputs : moves ;
  returns : zip (payoffAndGate penalty reward deposits (obfuscateAndGate moves)) bribesAccepted ;

  inputs : moves ;
  operation : fromFunctions (map not) id ;
  outputs : bribesAccepted ;
|]

-- andGateGameEq =
--   let numPlayers = 2
--       reward = 1.0
--       costOfCapital = 0.05
--       minBribe = 0.0
--       maxBribe = 5.0
--       incrementBribe = 0.05
--       maxSuccessfulAttackPayoff = 1000.0
--       attackerProbability = 1.0
--       penalty = 0.5
--       minDeposit = 0.0
--       maxDeposit = 10.0
--       incrementDeposit = 0.1
--       epsilon = 0.0
--    in equilibrium (andGateGame numPlayers reward costOfCapital minBribe maxBribe incrementBribe maxSuccessfulAttackPayoff attackerProbability penalty minDeposit maxDeposit incrementDeposit epsilon) void
--
-- depositStrategy :: [Kleisli Stochastic Double Double]
-- depositStrategy = replicate 2 (Kleisli (const (certainly 0.0)))
--
-- attackerStrategy :: Kleisli Stochastic ([Double], Double) Double
-- attackerStrategy = Kleisli (const (certainly 0.005))
--
-- playingStrategy :: [Kleisli Stochastic ([Double], Double) Bool]
-- playingStrategy = replicate 2 (Kleisli (\(_, bribe) -> if bribe > 0 then certainly False else certainly True))
--
-- -- Next: upgrade playing state to the smart strategy
-- -- set attacker probability to something intermediate
-- -- then min-max the penalty

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OpenGames.Examples.Consensus.DepositGame where

import Control.Arrow (Kleisli(..))
import Numeric.Probability.Distribution (certainly)

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision

depositStagePlayerSrc = Block ["costOfCapital"] []
  [Line ["costOfCapital"] [] "dependentDecision name (const [minDeposit, minDeposit + incrementDeposit .. maxDeposit])" ["deposit"] ["-deposit * costOfCapital"]]
  ["deposit"] []

depositStagePlayer name minDeposit maxDeposit incrementDeposit = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(costOfCapital, deposit) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\costOfCapital -> (costOfCapital, costOfCapital)) (\((costOfCapital, deposit), ()) -> (costOfCapital, deposit))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision name (const [minDeposit, minDeposit + incrementDeposit .. maxDeposit]))))))) >>> (fromFunctions (\(costOfCapital, deposit) -> (costOfCapital, deposit)) (\(costOfCapital, deposit) -> ((costOfCapital, deposit), -deposit * costOfCapital)))))))) >>> (fromLens (\(costOfCapital, deposit) -> deposit) (curry (\((costOfCapital, deposit), ()) -> (costOfCapital, deposit)))))

playingStagePlayerSrc = Block ["observation", "bribe"] []
  [Line ["observation", "bribe"] [] "dependentDecision name (const moves)" ["move"] ["payoff + if bribePaid then bribe else 0"]]
  ["move"] ["payoff", "bribePaid"]

playingStagePlayer name moves payoutCondition = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(observation, bribe, move, payoff, bribePaid) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(observation, bribe) -> ((observation, bribe), (observation, bribe))) (\((observation, bribe, move, payoff, bribePaid), ()) -> (observation, bribe, move, payoff, bribePaid))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision name (const moves))))))) >>> (fromFunctions (\((observation, bribe), move) -> (observation, bribe, move)) (\(observation, bribe, move, payoff, bribePaid) -> ((observation, bribe, move, payoff, bribePaid), payoff + if bribePaid then bribe else 0)))))))) >>> (fromLens (\(observation, bribe, move) -> move) (curry (\((observation, bribe, move), (payoff, bribePaid)) -> (observation, bribe, move, payoff, bribePaid)))))

class Obfuscatable x y where
  obfuscate :: [x] -> y

instance Obfuscatable Bool [Bool] where
  obfuscate xs = if numHonest >= numCensor then replicate numPlayers True else map not xs
    where numPlayers = length xs
          numHonest = length (filter id xs)
          numCensor = length (filter not xs)

instance Obfuscatable Bool Int where
  obfuscate xs = length (filter id xs)

payoffInt :: Double -> [Double] -> Int -> [Double]
payoffInt reward deposits numHonest = if totalDeposit == 0
                                        then replicate (length deposits) 0
                                        else [(reward*deposit*m) / (totalDeposit*n) | deposit <- deposits]
  where n = fromIntegral (length deposits)
        m = fromIntegral numHonest
        totalDeposit = sum deposits

attackerPayoff :: [Bool] -> Double -> Double -> Double
attackerPayoff bribesAccepted bribe successfulAttackPayoff
  | (numBribed == numPlayers) = successfulAttackPayoff - bribe*(fromIntegral numBribed)
  | (otherwise)               = -bribe*(fromIntegral numBribed)
  where numPlayers = length bribesAccepted
        numBribed  = length (filter id bribesAccepted)

fullThingSrc = Block [] []
  [Line ["replicate numPlayers costOfCapital"] ["discard1"] "population [depositStagePlayer (\"Player \" ++ show n) 0 10 0.1 | n <- [1 .. numPlayers]]" ["deposits"] ["replicate numPlayers ()"],
   Line ["deposits"] [] "dependentDecision \"Attacker\" (const [0, 0.1 .. maxBribe])" ["bribe"] ["attackerPayoff bribesAccepted bribe successfulAttackPayoff"],
   Line ["replicate numPlayers (deposits, bribe)"] ["discard2"] "population [playingStagePlayer (\"Player \" ++ show n) [True, False] (const False) | n <- [1 .. numPlayers]]" ["moves"] ["zip (payoffInt reward deposits (obfuscate moves)) bribesAccepted"],
   Line ["moves"] [] "fromFunctions (map not) id" ["bribesAccepted"] []]
  [] []

fullThing numPlayers reward costOfCapital maxBribe successfulAttackPayoff = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(deposits, bribe, moves, bribesAccepted, discard2, discard1) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), replicate numPlayers costOfCapital)) (\((deposits, bribe, moves, bribesAccepted, discard2), discard1) -> (deposits, bribe, moves, bribesAccepted, discard2, discard1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 | n <- [1 .. numPlayers]])))))) >>> (fromFunctions (\((), deposits) -> deposits) (\(deposits, bribe, moves, bribesAccepted, discard2) -> ((deposits, bribe, moves, bribesAccepted, discard2), replicate numPlayers ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\deposits -> (deposits, deposits)) (\((deposits, bribe, moves, bribesAccepted, discard2), ()) -> (deposits, bribe, moves, bribesAccepted, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Attacker" (const [0, 0.1 .. maxBribe]))))))) >>> (fromFunctions (\(deposits, bribe) -> (deposits, bribe)) (\(deposits, bribe, moves, bribesAccepted, discard2) -> ((deposits, bribe, moves, bribesAccepted, discard2), attackerPayoff bribesAccepted bribe successfulAttackPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(deposits, bribe) -> ((deposits, bribe), replicate numPlayers (deposits, bribe))) (\((deposits, bribe, moves, bribesAccepted), discard2) -> (deposits, bribe, moves, bribesAccepted, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((population [playingStagePlayer ("Player " ++ show n) [True, False] (const False) | n <- [1 .. numPlayers]])))))) >>> (fromFunctions (\((deposits, bribe), moves) -> (deposits, bribe, moves)) (\(deposits, bribe, moves, bribesAccepted) -> ((deposits, bribe, moves, bribesAccepted), zip (payoffInt reward deposits (obfuscate moves)) bribesAccepted)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(deposits, bribe, moves) -> ((deposits, bribe, moves), moves)) (\((deposits, bribe, moves, bribesAccepted), ()) -> (deposits, bribe, moves, bribesAccepted))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (map not) id)))))) >>> (fromFunctions (\((deposits, bribe, moves), bribesAccepted) -> (deposits, bribe, moves, bribesAccepted)) (\(deposits, bribe, moves, bribesAccepted) -> ((deposits, bribe, moves, bribesAccepted), ()))))))))) >>> (fromLens (\(deposits, bribe, moves, bribesAccepted) -> ()) (curry (\((deposits, bribe, moves, bribesAccepted), ()) -> (deposits, bribe, moves, bribesAccepted)))))

testFullThing costOfCapital maxBribe = equilibrium (fullThing 10 10 costOfCapital maxBribe 1000) void
-- with 10 players, reward = 5, costOfCapital = 0.046

deviationPenalty i reward deposits = ((payoffInt reward deposits numPlayers) !! i)
                                   - ((payoffInt reward deposits (numPlayers - 1)) !! i)
  where numPlayers = length deposits

bribeStrategy i reward = Kleisli $ \(deposits, bribe) -> certainly $ deviationPenalty i reward deposits >= bribe

testBribeStrategy costOfCapital maxBribe = testFullThing costOfCapital maxBribe $
  (replicate 10 $ Kleisli $ const $ certainly 9.8,
   Kleisli $ const $ certainly 0.1,
   [bribeStrategy i 10 | i <- [0 .. 9]],
   ())

-- next time: minmax the attacker, maximise attacker budget

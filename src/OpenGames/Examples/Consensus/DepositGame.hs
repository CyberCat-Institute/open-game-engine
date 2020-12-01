{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module OpenGames.Examples.Consensus.DepositGame where

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

playingStagePlayerSrc = Block ["deposit", "bribe"] []
  [Line ["deposit", "bribe"] [] "dependentDecision name (const moves)" ["move"] ["payoff + if bribePaid then bribe else 0"]]
  ["move"] ["payoff", "bribePaid"]

playingStagePlayer name moves payoutCondition = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(deposit, bribe, move, payoff, bribePaid) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(deposit, bribe) -> ((deposit, bribe), (deposit, bribe))) (\((deposit, bribe, move, payoff, bribePaid), ()) -> (deposit, bribe, move, payoff, bribePaid))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision name (const moves))))))) >>> (fromFunctions (\((deposit, bribe), move) -> (deposit, bribe, move)) (\(deposit, bribe, move, payoff, bribePaid) -> ((deposit, bribe, move, payoff, bribePaid), payoff + if bribePaid then bribe else 0)))))))) >>> (fromLens (\(deposit, bribe, move) -> move) (curry (\((deposit, bribe, move), (payoff, bribePaid)) -> (deposit, bribe, move, payoff, bribePaid)))))

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
payoffInt reward deposits numHonest = [(reward*deposit*m) / (totalDeposit*n) | deposit <- deposits]
  where n = fromIntegral (length deposits)
        m = fromIntegral numHonest
        totalDeposit = sum deposits

fullThingSrc = Block [] []
  [Line ["replicate numPlayers costOfCapital"] ["discard1"] "population [depositStagePlayer (\"Player \" ++ show n) 0 10 0.1 | n <- [1 .. numPlayers]]" ["deposits"] ["replicate numPlayers ()"],
   Line ["zip deposits (replicate numPlayers 0)"] ["discard2"] "population [playingStagePlayer (\"Player \" ++ show n) [True, False] (const False) | n <- [1 .. numPlayers]]" ["moves"] ["zip (payoffInt reward deposits (obfuscate moves)) (replicate numPlayers False)"]]
  [] []

fullThing numPlayers reward costOfCapital = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(deposits, moves, discard2, discard1) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), replicate numPlayers costOfCapital)) (\((deposits, moves, discard2), discard1) -> (deposits, moves, discard2, discard1))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 | n <- [1 .. numPlayers]])))))) >>> (fromFunctions (\((), deposits) -> deposits) (\(deposits, moves, discard2) -> ((deposits, moves, discard2), replicate numPlayers ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\deposits -> (deposits, zip deposits (replicate numPlayers 0))) (\((deposits, moves), discard2) -> (deposits, moves, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((population [playingStagePlayer ("Player " ++ show n) [True, False] (const False) | n <- [1 .. numPlayers]])))))) >>> (fromFunctions (\(deposits, moves) -> (deposits, moves)) (\(deposits, moves) -> ((deposits, moves), zip (payoffInt reward deposits (obfuscate moves)) (replicate numPlayers False)))))))))) >>> (fromLens (\(deposits, moves) -> ()) (curry (\((deposits, moves), ()) -> (deposits, moves)))))

testFullThing = equilibrium (fullThing 100 50 0) void

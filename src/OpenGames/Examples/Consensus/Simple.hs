{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.Consensus.Simple where

import Control.Arrow (Kleisli(..))
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.StatefulBayesian
import Numeric.Probability.Distribution
import Data.Ord (comparing)
import Data.List (maximumBy)

--type Stochastic = T Double

type AliceBobDecision = Bool
data CarolDecision = Punish | Reward deriving (Eq, Ord, Show)

type Stake = Double

costOfCapital :: Double
costOfCapital = 0.01

prize :: Stake -> Stake -> Double
prize a b = 5.0

reward :: Double -> (Stake, Stake, CarolDecision) -> (Double, Double)
reward penalty (a, b, Reward) = ((prize a b)*a/(a + b), (prize a b)*b/(a + b))
reward penalty (a, b, Punish) = (-penalty*a, -penalty*b)

carol :: AliceBobDecision -> CarolDecision
carol x = if x then Reward else Punish

andGateGame_src = Block ["aliceStake", "bobStake", "(bribe :: Double)"] []
  [Line Nothing ["\"Alice\"", "()"] [] "roleDecision [False, True]" ["aliceDecision"] ["alicePayoff"],
   Line Nothing ["\"Bob\"", "bribe"] [] "roleDecision [False, True]" ["bobDecision"] ["if bobDecision then bobPayoff else bobPayoff + bribe"],
   Line Nothing ["aliceStake", "bobStake", "carol (aliceDecision && bobDecision)"] ["()"] "fromFunctions (reward penalty) id" ["alicePayoff", "bobPayoff"] ["()"]]
  ["bobDecision"] []

andGateGame penalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(aliceStake, bobStake, (bribe :: Double)) -> ((aliceStake, bobStake, (bribe :: Double)), ("Alice", ()))) (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [False, True])))))) >>> (fromFunctions (\((aliceStake, bobStake, (bribe :: Double)), aliceDecision) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision)) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), alicePayoff))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(aliceStake, bobStake, (bribe :: Double), aliceDecision) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision), ("Bob", bribe))) (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [False, True])))))) >>> (fromFunctions (\((aliceStake, bobStake, (bribe :: Double), aliceDecision), bobDecision) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision)) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), if bobDecision then bobPayoff else bobPayoff + bribe)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision), (aliceStake, bobStake, carol (aliceDecision && bobDecision)))) (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (reward penalty) id)))))) >>> (fromFunctions (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision), (alicePayoff, bobPayoff)) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff)) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff), ()))))))))) >>> (fromLens (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff) -> bobDecision) (curry (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff)))))

consensusGame_src = Block ["bribe"] []
  [Line Nothing ["\"Alice\"", "()"] [] "roleDecision [0.0 .. 10.0]" ["aliceStake"] ["-costOfCapital*aliceStake"],
   Line Nothing ["\"Bob\"", "()"] [] "roleDecision [0.0 .. 10.0]" ["bobStake"] ["-costOfCapital*bobStake"],
   Line Nothing ["aliceStake", "bobStake", "bribe"] [] "andGateGame penalty" ["bobDecision"] []]
  ["bobDecision"] []

consensusGame penalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(bribe, aliceStake, bobStake, bobDecision) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\bribe -> (bribe, ("Alice", ()))) (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [0.0 .. 10.0])))))) >>> (fromFunctions (\(bribe, aliceStake) -> (bribe, aliceStake)) (\(bribe, aliceStake, bobStake, bobDecision) -> ((bribe, aliceStake, bobStake, bobDecision), -costOfCapital*aliceStake))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(bribe, aliceStake) -> ((bribe, aliceStake), ("Bob", ()))) (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [0.0 .. 10.0])))))) >>> (fromFunctions (\((bribe, aliceStake), bobStake) -> (bribe, aliceStake, bobStake)) (\(bribe, aliceStake, bobStake, bobDecision) -> ((bribe, aliceStake, bobStake, bobDecision), -costOfCapital*bobStake)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(bribe, aliceStake, bobStake) -> ((bribe, aliceStake, bobStake), (aliceStake, bobStake, bribe))) (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((andGateGame penalty)))))) >>> (fromFunctions (\((bribe, aliceStake, bobStake), bobDecision) -> (bribe, aliceStake, bobStake, bobDecision)) (\(bribe, aliceStake, bobStake, bobDecision) -> ((bribe, aliceStake, bobStake, bobDecision), ()))))))))) >>> (fromLens (\(bribe, aliceStake, bobStake, bobDecision) -> bobDecision) (curry (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision)))))

type Bribe = Double

prior :: Bribe -> Stochastic Bribe
prior bribeBudget = fromFreqs [(0.0, 1), (bribeBudget, 1)]

attackerGame_src = Block [] []
  [Line Nothing [] [] "nature (prior bribeBudget)" ["bribe"] [],
   Line Nothing ["bribe"] [] "consensusGame penalty" ["bobDecision"] []]
  [] []

attackerGame bribeBudget penalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(bribe, bobDecision) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((bribe, bobDecision), ()) -> (bribe, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (prior bribeBudget))))))) >>> (fromFunctions (\((), bribe) -> bribe) (\(bribe, bobDecision) -> ((bribe, bobDecision), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\bribe -> (bribe, bribe)) (\((bribe, bobDecision), ()) -> (bribe, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((consensusGame penalty)))))) >>> (fromFunctions (\(bribe, bobDecision) -> (bribe, bobDecision)) (\(bribe, bobDecision) -> ((bribe, bobDecision), ()))))))))) >>> (fromLens (\(bribe, bobDecision) -> ()) (curry (\((bribe, bobDecision), ()) -> (bribe, bobDecision)))))

attackerGame_eq a b c d bribeBudget penalty = equilibrium (attackerGame bribeBudget penalty) void ((), (Kleisli (const a), Kleisli (const b), (Kleisli (const c), Kleisli (certainly . d), ())))

minimumBribeBudget :: Double -> Double
minimumBribeBudget penalty = minimum [bribeBudget | bribeBudget <- [0.0 .. 15.0], not (null (attackerGame_eq (certainly 10.0) (certainly 10.0) (certainly True) (<= 10.0) bribeBudget penalty))]

penaltiesWithMinimumBudget :: [(Double, Double)]
penaltiesWithMinimumBudget = [(penalty, minimumBribeBudget penalty) | penalty <- Prelude.map (/10) [1..10]]

minimumPenaltyWithMinmaxBudget :: (Double, Double)
minimumPenaltyWithMinmaxBudget = minimum (Prelude.filter (\(x,y) -> y == minmaxBudget) penaltiesWithMinimumBudget)
  where minmaxBudget = snd (maximumBy (comparing snd) penaltiesWithMinimumBudget)

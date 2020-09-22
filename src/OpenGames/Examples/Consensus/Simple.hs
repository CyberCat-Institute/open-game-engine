{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.Consensus.Simple where

import Control.Arrow (Kleisli(..))
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.StatefulBayesian
import Numeric.Probability.Distribution

--type Stochastic = T Double

type AliceBobDecision = Bool
data CarolDecision = Punish | Reward deriving (Eq, Ord, Show)

type Stake = Double

costOfCapital :: Double
costOfCapital = 0.01

penalty :: Double
penalty = 1.0

reward :: (Stake, Stake, CarolDecision) -> (Double, Double)
reward (a, b, Reward) = (a/(a + b), b/(a + b))
reward (a, b, Punish) = (-penalty*a, -penalty*b)

carol :: AliceBobDecision -> CarolDecision
carol x = if x then Reward else Punish

andGateGame_src = Block ["aliceStake", "bobStake", "(bribe :: Double)"] []
  [Line ["\"Alice\"", "()"] [] "roleDecision [False, True]" ["aliceDecision"] ["alicePayoff"],
   Line ["\"Bob\"", "bribe"] [] "roleDecision [False, True]" ["bobDecision"] ["if bobDecision then bobPayoff else bobPayoff + bribe"],
   Line ["aliceStake", "bobStake", "carol (aliceDecision && bobDecision)"] ["()"] "fromFunctions reward id" ["alicePayoff", "bobPayoff"] ["()"]]
  ["bobDecision"] []

andGateGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(aliceStake, bobStake, (bribe :: Double)) -> ((aliceStake, bobStake, (bribe :: Double)), ("Alice", ()))) (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [False, True])))))) >>> (fromFunctions (\((aliceStake, bobStake, (bribe :: Double)), aliceDecision) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision)) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), alicePayoff))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(aliceStake, bobStake, (bribe :: Double), aliceDecision) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision), ("Bob", bribe))) (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [False, True])))))) >>> (fromFunctions (\((aliceStake, bobStake, (bribe :: Double), aliceDecision), bobDecision) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision)) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()), if bobDecision then bobPayoff else bobPayoff + bribe)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision), (aliceStake, bobStake, carol (aliceDecision && bobDecision)))) (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions reward id)))))) >>> (fromFunctions (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision), (alicePayoff, bobPayoff)) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff)) (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff) -> ((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff), ()))))))))) >>> (fromLens (\(aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff) -> bobDecision) (curry (\((aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff), ()) -> (aliceStake, bobStake, (bribe :: Double), aliceDecision, bobDecision, alicePayoff, bobPayoff)))))

consensusGame_src = Block ["bribe"] []
  [Line ["\"Alice\"", "()"] [] "roleDecision [0.0 .. 10.0]" ["aliceStake"] ["-costOfCapital*aliceStake"],
   Line ["\"Bob\"", "()"] [] "roleDecision [0.0 .. 10.0]" ["bobStake"] ["-costOfCapital*bobStake"],
   Line ["aliceStake", "bobStake", "bribe"] [] "andGateGame" ["bobDecision"] []]
  ["bobDecision"] []

consensusGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(bribe, aliceStake, bobStake, bobDecision) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\bribe -> (bribe, ("Alice", ()))) (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [0.0 .. 10.0])))))) >>> (fromFunctions (\(bribe, aliceStake) -> (bribe, aliceStake)) (\(bribe, aliceStake, bobStake, bobDecision) -> ((bribe, aliceStake, bobStake, bobDecision), -costOfCapital*aliceStake))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(bribe, aliceStake) -> ((bribe, aliceStake), ("Bob", ()))) (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [0.0 .. 10.0])))))) >>> (fromFunctions (\((bribe, aliceStake), bobStake) -> (bribe, aliceStake, bobStake)) (\(bribe, aliceStake, bobStake, bobDecision) -> ((bribe, aliceStake, bobStake, bobDecision), -costOfCapital*bobStake)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(bribe, aliceStake, bobStake) -> ((bribe, aliceStake, bobStake), (aliceStake, bobStake, bribe))) (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((andGateGame)))))) >>> (fromFunctions (\((bribe, aliceStake, bobStake), bobDecision) -> (bribe, aliceStake, bobStake, bobDecision)) (\(bribe, aliceStake, bobStake, bobDecision) -> ((bribe, aliceStake, bobStake, bobDecision), ()))))))))) >>> (fromLens (\(bribe, aliceStake, bobStake, bobDecision) -> bobDecision) (curry (\((bribe, aliceStake, bobStake, bobDecision), ()) -> (bribe, aliceStake, bobStake, bobDecision)))))

type Bribe = Double

attackerGame_src = Block [] []
  [Line [] [] "nature (uniform [0.0, 11.0])" ["bribe"] [],
   Line ["bribe"] [] "consensusGame" ["bobDecision"] []]
  [] []

attackerGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(bribe, bobDecision) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((bribe, bobDecision), ()) -> (bribe, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0.0, 11.0]))))))) >>> (fromFunctions (\((), bribe) -> bribe) (\(bribe, bobDecision) -> ((bribe, bobDecision), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\bribe -> (bribe, bribe)) (\((bribe, bobDecision), ()) -> (bribe, bobDecision))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((consensusGame)))))) >>> (fromFunctions (\(bribe, bobDecision) -> (bribe, bobDecision)) (\(bribe, bobDecision) -> ((bribe, bobDecision), ()))))))))) >>> (fromLens (\(bribe, bobDecision) -> ()) (curry (\((bribe, bobDecision), ()) -> (bribe, bobDecision)))))

attackerGame_eq a b c d = equilibrium attackerGame void ((), (Kleisli (const (certainly a)), Kleisli (const (certainly b)), (Kleisli (const (certainly c)), Kleisli (const (certainly d)), ())))

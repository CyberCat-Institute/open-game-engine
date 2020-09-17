module OpenGames.Examples.Consensus.Simple where

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Engine.BayesianDiagnostics
import Numeric.Probability.Distribution

type Stochastic = T Double

type AliceBobDecision = Bool
data CarolDecision = Punish | Reward deriving (Eq, Ord, Show)

aliceSignal, bobSignal :: Stochastic Double
aliceSignal = certainly 0
bobSignal = certainly 0

payoffs :: CarolDecision -> Double
payoffs Reward = 1
payoffs Punish = 0

carol :: (AliceBobDecision, AliceBobDecision) -> CarolDecision
carol (x, y) = if x && y then Reward else Punish

simpleConsensus_src = Block [] []
  [Line [] [] "nature aliceSignal" ["typeAlice"] [],
   Line [] [] "nature bobSignal" ["typeBob"] [],
   Line ["typeAlice"] [] "decision \"Alice\" [False, True]" ["aliceDecision"] ["payoffs carolDecision"],
   Line ["typeBob"] [] "decision \"Bob\" [False, True]" ["bobDecision"] ["payoffs carolDecision"],
   Line ["aliceDecision", "bobDecision"] ["()"] "fromFunctions carol id" ["carolDecision"] ["()"]]
  [] []

simpleConsensus = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), ()) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature aliceSignal)))))) >>> (fromFunctions (\((), typeAlice) -> typeAlice) (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()) -> ((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\typeAlice -> (typeAlice, ())) (\((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), ()) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature bobSignal)))))) >>> (fromFunctions (\(typeAlice, typeBob) -> (typeAlice, typeBob)) (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()) -> ((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(typeAlice, typeBob) -> ((typeAlice, typeBob), typeAlice)) (\((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), ()) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "Alice" [False, True])))))) >>> (fromFunctions (\((typeAlice, typeBob), aliceDecision) -> (typeAlice, typeBob, aliceDecision)) (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()) -> ((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), payoffs carolDecision)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(typeAlice, typeBob, aliceDecision) -> ((typeAlice, typeBob, aliceDecision), typeBob)) (\((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), ()) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "Bob" [False, True])))))) >>> (fromFunctions (\((typeAlice, typeBob, aliceDecision), bobDecision) -> (typeAlice, typeBob, aliceDecision, bobDecision)) (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()) -> ((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()), payoffs carolDecision)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(typeAlice, typeBob, aliceDecision, bobDecision) -> ((typeAlice, typeBob, aliceDecision, bobDecision), (aliceDecision, bobDecision))) (\((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision), ()) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision, ()))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions carol id)))))) >>> (fromFunctions (\((typeAlice, typeBob, aliceDecision, bobDecision), carolDecision) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision)) (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision) -> ((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision), ()))))))))) >>> (fromLens (\(typeAlice, typeBob, aliceDecision, bobDecision, carolDecision) -> ()) (curry (\((typeAlice, typeBob, aliceDecision, bobDecision, carolDecision), ()) -> (typeAlice, typeBob, aliceDecision, bobDecision, carolDecision)))))

simpleConsensus_eq = equilibrium simpleConsensus trivialContext

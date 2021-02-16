{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

module OpenGames.Examples.Consensus.DepositGame where

import Control.Arrow (Kleisli(..))
import Numeric.Probability.Distribution (certainly, uniform, fromFreqs)

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision


generateGame "depositStagePlayer" ["name", "minDeposit", "maxDeposit", "incrementDeposit", "epsilon"] $
  block ["costOfCapital"] []
  [line [[|costOfCapital|]] [] [|epsilonDecision epsilon name [minDeposit, minDeposit + incrementDeposit .. maxDeposit]|] ["deposit"] [[|(-deposit) * costOfCapital|]]]
  [[|deposit|]] []
{-}
depositStagePlayerSrc = Block ["costOfCapital"] []
  [Line ["costOfCapital"] [] "dependentDecision name (const [minDeposit, minDeposit + incrementDeposit .. maxDeposit])" ["deposit"] ["-deposit * costOfCapital"]]
  ["deposit"] []

depositStagePlayer name minDeposit maxDeposit incrementDeposit = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(costOfCapital, deposit) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\costOfCapital -> (costOfCapital, costOfCapital)) (\((costOfCapital, deposit), ()) -> (costOfCapital, deposit))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision name (const [minDeposit, minDeposit + incrementDeposit .. maxDeposit]))))))) >>> (fromFunctions (\(costOfCapital, deposit) -> (costOfCapital, deposit)) (\(costOfCapital, deposit) -> ((costOfCapital, deposit), -deposit * costOfCapital)))))))) >>> (fromLens (\(costOfCapital, deposit) -> deposit) (curry (\((costOfCapital, deposit), ()) -> (costOfCapital, deposit)))))
-}
generateGame "playingStagePlayer" ["name", "moves"] $ block ["observation", "bribe"] []
  [line [[|observation|], [|bribe|]] [] [|dependentDecision name (const moves)|] ["move"] [[|payoff + if bribePaid then bribe else 0|]]]
  [[|move|]] ["payoff", "bribePaid"]

class Obfuscatable x y where
  obfuscate :: [x] -> y

instance Obfuscatable Bool [Bool] where
  obfuscate xs = if numHonest >= numCensor then replicate numPlayers True else map not xs
    where numPlayers = length xs
          numHonest = length (filter id xs)
          numCensor = length (filter not xs)

instance Obfuscatable Bool Int where
  obfuscate xs = length (filter id xs)

instance Obfuscatable (Bool, Double) Double where
  obfuscate xs = sum [weight | (True, weight) <- xs]

payoffInt :: Double -> Double -> [Double] -> Int -> [Double]
payoffInt safeDepositProportion reward deposits numHonest
  = if totalDeposit == 0
       then replicate (length deposits) 0
       else [(rewardShare deposit + deposit)*payoffScaler - deposit | deposit <- deposits]
  where n = fromIntegral (length deposits)
        m = fromIntegral numHonest
        totalDeposit = sum deposits
        outcomeScore = m / n
        payoffScaler = outcomeScore*(1 - safeDepositProportion) + safeDepositProportion
        rewardShare deposit = (deposit / totalDeposit)*reward

payoffWeightedDeposits :: Double -> Double -> [Double] -> Double -> [Double]
payoffWeightedDeposits safeDepositProportion reward deposits sumHonest
  = [(deposit * sumHonest * reward) / (totalDeposits * totalDeposits) | deposit <- deposits]
  where totalDeposits = sum deposits
  -- TODO deal with totalDeposit==0, then hook it into the game

attackerPayoff :: [Bool] -> Double -> Double -> Double
attackerPayoff bribesAccepted bribe successfulAttackPayoff
  | (numBribed == numPlayers) = successfulAttackPayoff - bribe*(fromIntegral numBribed)
  | (otherwise)               = -bribe*(fromIntegral numBribed)
  where numPlayers = length bribesAccepted
        numBribed  = length (filter id bribesAccepted)

generateGame "completeGame" ["numPlayers", "reward", "costOfCapital", "maxBribe", "successfulAttackPayoff", "safeDepositProportion"] $ block [] []
  [line [ [| replicate numPlayers costOfCapital |] ] ["discard1"] [| population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 0.001 | n <- [1 .. numPlayers]] |] ["deposits"] [ [| replicate numPlayers () |] ],
   line [ [| deposits |] ] [] [| dependentDecision "Attacker" (const [0, 0.025 .. maxBribe]) |] ["bribe"] [ [| attackerPayoff bribesAccepted bribe successfulAttackPayoff |] ],
   line [ [| replicate numPlayers (deposits, bribe) |] ] ["discard2"] [| population [playingStagePlayer ("Player " ++ show n) [True, False] | n <- [1 .. numPlayers]] |] ["moves"] [ [| zip (payoffInt safeDepositProportion reward deposits (obfuscate moves)) bribesAccepted |] ],
   line [ [| moves |] ] [] [| fromFunctions (map not) id |] ["bribesAccepted"] []]
  [] []

generateGame "randomAttacker" ["numPlayers", "reward", "costOfCapital", "maxBribe", "maxSuccessfulAttackPayoff", "payoffParameter"] $ block [] []
  [line [ [| replicate numPlayers costOfCapital |] ] ["discard1"] [| population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 0.001 | n <- [1 .. numPlayers]] |] ["deposits"] [ [| replicate numPlayers () |] ],
   line [] [] [| nature (fromFreqs [(0, 0.05), (maxSuccessfulAttackPayoff, 0.95)]) |] ["successfulAttackPayoff"] [],
   line [ [| deposits |], [| successfulAttackPayoff |] ] [] [| dependentDecision "Attacker" (const [0, 0.025 .. maxBribe]) |] ["bribe"] [ [| attackerPayoff bribesAccepted bribe successfulAttackPayoff |] ],
   line [ [| replicate numPlayers (deposits, bribe) |] ] ["discard2"] [| population [playingStagePlayer ("Player " ++ show n) [True, False] | n <- [1 .. numPlayers]] |] ["moves"] [ [| zip (payoffInt payoffParameter reward deposits (obfuscate moves)) bribesAccepted |] ],
   line [ [| moves |] ] [] [| fromFunctions (map not) id |] ["bribesAccepted"] []]
  [] []

  {- attacker needs to be an EpsilonDecision !!! -}

---------
-- replacing the attacker by an epsilon decision

generateGame "completeGameEpsilon" ["numPlayers", "reward", "costOfCapital", "maxBribe", "successfulAttackPayoff", "safeDepositProportion", "epsilonAtt"] $ block [] []
  [line [ [| replicate numPlayers costOfCapital |] ] ["discard1"] [| population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 0.001 | n <- [1 .. numPlayers]] |] ["deposits"] [ [| replicate numPlayers () |] ],
   line [ [| deposits |] ] [] [| epsilonDecision epsilonAtt "Attacker" [0, 0.025 .. maxBribe] |] ["bribe"] [ [| attackerPayoff bribesAccepted bribe successfulAttackPayoff |] ],
   line [ [| replicate numPlayers (deposits, bribe) |] ] ["discard2"] [| population [playingStagePlayer ("Player " ++ show n) [True, False] | n <- [1 .. numPlayers]] |] ["moves"] [ [| zip (payoffInt safeDepositProportion reward deposits (obfuscate moves)) bribesAccepted |] ],
   line [ [| moves |] ] [] [| fromFunctions (map not) id |] ["bribesAccepted"] []]
  [] []

generateGame "randomAttackerEpsilon" ["numPlayers", "reward", "costOfCapital", "maxBribe", "maxSuccessfulAttackPayoff", "payoffParameter", "epsilonAtt"] $ block [] []
  [line [ [| replicate numPlayers costOfCapital |] ] ["discard1"] [| population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 0.001 | n <- [1 .. numPlayers]] |] ["deposits"] [ [| replicate numPlayers () |] ],
   line [] [] [| nature (fromFreqs [(0, 0.05), (maxSuccessfulAttackPayoff, 0.95)]) |] ["successfulAttackPayoff"] [],
   line [ [| (deposits,successfulAttackPayoff) |] ] [] [| epsilonDecision epsilonAtt "Attacker" [0, 0.025 .. maxBribe] |] ["bribe"] [ [| attackerPayoff bribesAccepted bribe successfulAttackPayoff |] ],
   line [ [| replicate numPlayers (deposits, bribe) |] ] ["discard2"] [| population [playingStagePlayer ("Player " ++ show n) [True, False] | n <- [1 .. numPlayers]] |] ["moves"] [ [| zip (payoffInt payoffParameter reward deposits (obfuscate moves)) bribesAccepted |] ],
   line [ [| moves |] ] [] [| fromFunctions (map not) id |] ["bribesAccepted"] []]
  [] []

-- To explore the effect of different distributions on players
generateGame "randomAttackerEpsilonProb" ["numPlayers", "reward", "costOfCapital", "maxBribe", "maxSuccessfulAttackPayoff", "payoffParameter", "epsilonAtt", "prob0Attacker"] $ block [] []
  [line [ [| replicate numPlayers costOfCapital |] ] ["discard1"] [| population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 0.001 | n <- [1 .. numPlayers]] |] ["deposits"] [ [| replicate numPlayers () |] ],
   line [] [] [| nature (fromFreqs [(0, prob0Attacker), (maxSuccessfulAttackPayoff, (1.00 - prob0Attacker))]) |] ["successfulAttackPayoff"] [],
   line [ [| (deposits,successfulAttackPayoff) |] ] [] [| epsilonDecision epsilonAtt "Attacker" [0, 0.025 .. maxBribe] |] ["bribe"] [ [| attackerPayoff bribesAccepted bribe successfulAttackPayoff |] ],
   line [ [| replicate numPlayers (deposits, bribe) |] ] ["discard2"] [| population [playingStagePlayer ("Player " ++ show n) [True, False] | n <- [1 .. numPlayers]] |] ["moves"] [ [| zip (payoffInt payoffParameter reward deposits (obfuscate moves)) bribesAccepted |] ],
   line [ [| moves |] ] [] [| fromFunctions (map not) id |] ["bribesAccepted"] []]
  [] []



------------------
-- Analysis


testFullThing numPlayers reward costOfCapital = equilibrium (completeGame numPlayers reward costOfCapital 10 1000 0) void
-- with 10 players, reward = 5, costOfCapital = 0.046

-- difference in payoffs if player i deviates from playing truthful
deviationPenalty i reward deposits safeDepositProportion = ((payoffInt safeDepositProportion reward deposits numPlayers) !! i)
                                   - ((payoffInt safeDepositProportion reward deposits (numPlayers - 1)) !! i)
  where numPlayers = length deposits

-- Play true if payoff from being bribed is smaller than playing truthful
bribeStrategy i reward safeDepositProportion = Kleisli $ \(deposits, bribe) -> certainly $ deviationPenalty i reward deposits safeDepositProportion >= bribe

testBribeStrategy costOfCapital bribe safeDepositProportion = testFullThing numPlayers reward costOfCapital $
  (replicate numPlayers $ Kleisli $ const $ certainly 5,
   Kleisli $ const $ certainly bribe,
   [bribeStrategy i reward safeDepositProportion | i <- [0 .. numPlayers - 1]],
   ())
   where reward = 5
         numPlayers = 10

-- Analysis
-- 0 Starting point: No bribe; finding an equilibrium with interior solutions
-- equilibrium of complete game; minDeposit = 0, maxDeposit = 10, steps = 0.1, epsilon = 0.01
equilibriumCompleteGame numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion = equilibrium (completeGame numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion ) void


-- Sanity tests with 2 players
-- with 2 players, reward = 1, (NOTE depositMax 10), maxBribe= 20, successfulAttackPayoff=1000, safeDepositProportion=0
test2player costOfCapital = equilibriumCompleteGame 2 1 costOfCapital 20 1000 0


-- NE at  test2Strategy 0.05 5 5 0
-- NE breaks down at bribe=2.8
test2Strategy coc deposit1 deposit2 bribe =
  test2player
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , Kleisli $ const $ certainly bribe
     , [(Kleisli $ const $ certainly True), (Kleisli $ const $ certainly True)]
     , ())

-- test strategy where players always accept bribe
test2Strategy' coc deposit1 deposit2 bribe =
  test2player
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , Kleisli $ const $ certainly bribe
     , [(Kleisli $ const $ certainly False), (Kleisli $ const $ certainly False)]
     , ())



-- semi-smart strategy accepts profitable bribes for coc=0.05 + params of test2player
-- models a successful attack - NE for deposit1=deposit2=0
test2StrategySemi coc deposit1 deposit2 bribe =
  test2player
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
    , Kleisli $ const $ certainly bribe
    , [Kleisli strategy, Kleisli strategy]
    , ())
    where strategy (_, bribe) = certainly $ if bribe >= 2.8 then False else True

-- 10 players, reward = 20/9, (NOTE depositMax 10),  maxBribe= 20, successfulAttackPayoff=1000, safeDepositProportion=0
test10players costOfCapital =  equilibriumCompleteGame 10 (20/9) costOfCapital  20 1000 0


-- NE at test10Strategy 0.05 4 0
test10Strategy costOfCapital depositN bribe =  test10players costOfCapital
  (replicate 10 $ Kleisli $ const $ certainly 4
   , Kleisli $ const $ certainly bribe
   , replicate 10 $ Kleisli $ const $ certainly True
   , ()
   )




--- Understanding the different strategies and their behavior
-- Test smart strategy
test2SmartStrategy coc deposit1 deposit2 bribe =
  test2player
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , Kleisli $ const $ certainly bribe
     , [bribeStrategy i 1 0 | i <- [0,1]]
     , ())

-- Test smart strategy against naive strategy
test3SmartStrategy coc deposit1 deposit2 bribe =
  test2player
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , Kleisli $ const $ certainly bribe
     , [Kleisli strategy, Kleisli strategy]
     , ())
    where strategy (_, bribe) = certainly $ if bribe >= 0.5 then False else True


equilibriumRandomAttacker numPlayers reward costOfCapital maxBribe maxSuccessfulAttackPayoff safeDepositProportion = equilibrium (randomAttacker numPlayers reward costOfCapital maxBribe maxSuccessfulAttackPayoff safeDepositProportion) void

test2playerRandomAttacker costOfCapital = equilibriumRandomAttacker 2 1 costOfCapital 20 1000 0


test2StrategyRandom coc deposit1 deposit2 bribe =
  test2playerRandomAttacker
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , ()
     , Kleisli $ const $ certainly bribe
     , [(Kleisli $ const $ certainly True), (Kleisli $ const $ certainly True)]
     , ())



test2SmartStrategyRandomAttacker coc deposit1 deposit2 bribe =
       test2playerRandomAttacker
         coc
         ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
         , ()
         , Kleisli $ \(_, successfulAttackPayoff) -> case successfulAttackPayoff of
                                                       0 -> certainly 0
                                                       1000 -> certainly bribe
         , [bribeStrategy 0 1 0, bribeStrategy 1 1 0]
         , ())


----------------------------------------------
-- 1 Redoing the analysis for epsilon attacker
-- 1.0. Attacker with certainty
equilibriumEpsilonCertain numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion epsilonAtt= equilibrium (completeGameEpsilon numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion epsilonAtt) void

test2playerEpsilonCertain costOfCapital = equilibriumEpsilonCertain 2 1 costOfCapital 20 1000 0 0.001

-- Naive strategy (fixing behavior independent of observations)
test2StrategyEpsilonCertain coc deposit1 deposit2 bribe =
  test2playerEpsilonCertain
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , Kleisli $ const $ certainly bribe
     , [(Kleisli $ const $ certainly True), (Kleisli $ const $ certainly True)]
     , ())

-- Players adapting their strategy given bribe
test2SmartStrategyEpsilonCertain coc deposit1 deposit2 bribe =
  test2playerEpsilonCertain
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , Kleisli $ const $ certainly bribe
     , [bribeStrategy i 1 0 | i <- [0,1]]
     , ())

-- 1.1 Random attacker
equilibriumEpsilonRandom numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion epsilonAtt= equilibrium (randomAttackerEpsilon numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion epsilonAtt) void

test2playerEpsilonRandom costOfCapital = equilibriumEpsilonRandom 2 1 costOfCapital 20 1000 0 0.1

-- Naive strategy (fixing behavior independent of observations)
test2StrategyEpsilonRandom coc deposit1 deposit2 bribe =
  test2playerEpsilonRandom
    coc
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , ()
     , Kleisli $ const $ certainly bribe
     , [(Kleisli $ const $ certainly True), (Kleisli $ const $ certainly True)]
     , ())


-- Players adapting their strategy given bribe
test2SmartStrategyEpsilonRandom coc deposit1 deposit2 bribe =
       test2playerEpsilonRandom
         coc
         ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
         , ()
         , Kleisli $ \(_, successfulAttackPayoff) -> case successfulAttackPayoff of
                                                       0 -> certainly 0
                                                       1000 -> certainly bribe
         , [bribeStrategy i 1 0 | i <- [0,1]]
         , ())

-- 1.2 Random attacker with distribution parameters as inputs
equilibriumEpsilonRandomProb numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion epsilonAtt prob0Att= equilibrium (randomAttackerEpsilonProb numPlayers reward costOfCapital maxBribe successfulAttackPayoff safeDepositProportion epsilonAtt prob0Att)  void

test2playerEpsilonRandomProb costOfCapital prob = equilibriumEpsilonRandomProb 2 1 costOfCapital 20 1000 0 0.001 prob

-- Naive strategy (fixing behavior independent of observations)
test2StrategyEpsilonRandomProb coc deposit1 deposit2 bribe prob =
  test2playerEpsilonRandomProb
    coc
    prob
    ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
     , ()
     , Kleisli $ const $ certainly bribe
     , [(Kleisli $ const $ certainly True), (Kleisli $ const $ certainly True)]
     , ())


-- Players adapting their strategy given bribe
test2SmartStrategyEpsilonRandomProb coc deposit1 deposit2 bribe prob=
       test2playerEpsilonRandomProb
         coc
         prob
         ([(Kleisli $ const $ certainly deposit1),(Kleisli $ const $ certainly deposit2)] -- deposit
         , ()
         , Kleisli $ \(_, successfulAttackPayoff) -> case successfulAttackPayoff of
                                                       0 -> certainly 0
                                                       1000 -> certainly bribe
         , [bribeStrategy i 1 0 | i <- [0,1]]
         , ())




--- Systematic comparison of epsilon and non-epsilon attacker
-- NOTE we should get rid of the non-epsilon once we are clear about this


{-
--NE without bribe
λ> test2Strategy 0.05 5 5 0
[]
λ> test2StrategyEpsilonCertain 0.05 5 5 0
[]
λ>

--NE breaks down with bribe 2.8
λ> test2Strategy 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 2", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"}]
λ> test2StrategyEpsilonCertain 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 2", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"}]



-- NOTE pathological NE
λ> test2Strategy' 0.05 0 0 0
[]

--No bribe not an eq with smart player strategies

λ> test2SmartStrategy 0.05 5 5 0
[DiagnosticInfo {player = "Attacker", state = "[5.0,5.0]", unobservableState = "((),[5.0,5.0])", strategy = "fromFreqs [(0.0,1.0)]", payoff = "0.0", optimalMove = "2.7750000000000004", optimalPayoff = "994.45"}]
λ> test2SmartStrategyEpsilonCertain 0.05 5 5 0
[DiagnosticInfo {player = "Attacker", state = "[5.0,5.0]", unobservableState = "((),[5.0,5.0])", strategy = "fromFreqs [(0.0,1.0)]", payoff = "0.0", optimalMove = "2.7750000000000004", optimalPayoff = "994.45"}]

--Bribe with 2.8 not optimal for briber anymore (under smart strategy) -- independent of epsilonDecision or not
--NOTE I also checked against different approximation errors - to no effect, the attacker has an incentive to deviate

λ> test2SmartStrategy 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.45", optimalMove = "0.0", optimalPayoff = "2.8"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.45", optimalMove = "0.0", optimalPayoff = "2.8"},DiagnosticInfo {player = "Attacker", state = "[5.0,5.0]", unobservableState = "((),[5.0,5.0])", strategy = "fromFreqs [(2.8,1.0)]", payoff = "994.4", optimalMove = "2.7750000000000004", optimalPayoff = "994.45"}]
λ> test2SmartStrategyEpsilonCertain 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.45", optimalMove = "0.0", optimalPayoff = "2.8"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.45", optimalMove = "0.0", optimalPayoff = "2.8"},DiagnosticInfo {player = "Attacker", state = "[5.0,5.0]", unobservableState = "((),[5.0,5.0])", strategy = "fromFreqs [(2.8,1.0)]", payoff = "994.4", optimalMove = "2.7750000000000004", optimalPayoff = "994.45"}]


-- Introducting the bribe of above makes the attacker happy
λ> test2SmartStrategy 0.05 5 5 2.775
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.475", optimalMove = "0.0", optimalPayoff = "2.775"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.475", optimalMove = "0.0", optimalPayoff = "2.775"}]
λ> test2SmartStrategyEpsilonCertain 0.05 5 5 2.775
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.475", optimalMove = "0.0", optimalPayoff = "2.775"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.475", optimalMove = "0.0", optimalPayoff = "2.775"}]

-- Turn to random attacker
-- Replicate finding of NE with no bribe


λ> test2StrategyRandom 0.05 5 5 0
[]
λ> test2StrategyEpsilonRandom 0.05 5 5 0
[]

-- With positive bribe naively playing True not optimal -- accepting bribe would be better
λ> test2StrategyRandom 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],0.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 1", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],1000.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 2", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],0.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 2", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],1000.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"}]
λ> test2StrategyEpsilonRandom 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],0.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 1", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],1000.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 2", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],0.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"},DiagnosticInfo {player = "Player 2", state = "([5.0,5.0],2.8)", unobservableState = "((((),([5.0,5.0],1000.0,2.8)),[([5.0,5.0],2.8)]),([5.0,5.0],2.8))", strategy = "fromFreqs [(True,1.0)]", payoff = "0.5", optimalMove = "False", optimalPayoff = "0.5499999999999998"}]

-- Smart reaction strategy to bribe= 2.8 -- bribe is too high
-- NOTE no difference across the different versions of attacker

λ> test2SmartStrategyRandomAttacker 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.315", optimalMove = "0.0", optimalPayoff = "2.6599999999999997"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.315", optimalMove = "0.0", optimalPayoff = "2.6599999999999997"},DiagnosticInfo {player = "Attacker", state = "([5.0,5.0],1000.0)", unobservableState = "((),([5.0,5.0],1000.0))", strategy = "fromFreqs [(2.8,1.0)]", payoff = "994.4", optimalMove = "2.7750000000000004", optimalPayoff = "994.45"}]
λ> test2SmartStrategyEpsilonRandom 0.05 5 5 2.8
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.315", optimalMove = "0.0", optimalPayoff = "2.6599999999999997"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.315", optimalMove = "0.0", optimalPayoff = "2.6599999999999997"},DiagnosticInfo {player = "Attacker", state = "([5.0,5.0],1000.0)", unobservableState = "((),([5.0,5.0],1000.0))", strategy = "fromFreqs [(2.8,1.0)]", payoff = "994.4", optimalMove = "2.7750000000000004", optimalPayoff = "994.45"}

-- The bribe where the attacker has no incentive to deviate is bribe=2.775
-- NOTE: This is exactly the same value as above for the non random case. What is affected is expected value for the deposits
λ> test2SmartStrategyEpsilonRandom 0.05 5 5 2.775
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.3387499999999997", optimalMove = "0.0", optimalPayoff = "2.63625"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.3387499999999997", optimalMove = "0.0", optimalPayoff = "2.63625"}]

-- Exploring the distribution parameters
λ>  mapM_ print $ test2SmartStrategyEpsilonRandomProb 0.05 5 5 2.775 <$> [0, 0.05 .. 0.95]
-- relevant parts of output

...
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-0.43125", optimalMove = "0.0", optimalPayoff = "0.69375"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-0.43125", optimalMove = "0.0", optimalPayoff = "0.69375"}]
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-0.2949999999999999", optimalMove = "0.5", optimalPayoff = "0.5618181818181817"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-0.2949999999999999", optimalMove = "0.5", optimalPayoff = "0.5618181818181817"}]
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-0.15874999999999978", optimalMove = "1.1", optimalPayoff = "0.44555327868852435"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-0.15874999999999978", optimalMove = "1.1", optimalPayoff = "0.44555327868852435"}]
[DiagnosticInfo {player = "Player 1", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.2499999999999937e-2", optimalMove = "1.9000000000000001", optimalPayoff = "0.34909420289855075"},DiagnosticInfo {player = "Player 2", state = "5.0e-2", unobservableState = "((((),()),[5.0e-2]),5.0e-2)", strategy = "fromFreqs [(5.0,1.0)]", payoff = "-2.2499999999999937e-2", optimalMove = "1.9000000000000001", optimalPayoff = "0.34909420289855075"}]
...


-- NE with successful attack
-- NOTE different strategies
λ> test3SmartStrategy 0.05 0 0 0.5
[]

λ> test3SmartStrategy 0.05 0 0 0.6
[]

-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

module OpenGames.Examples.Consensus.DepositGame where

import Control.Arrow (Kleisli(..))
import Numeric.Probability.Distribution (certainly)

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

attackerPayoff :: [Bool] -> Double -> Double -> Double
attackerPayoff bribesAccepted bribe successfulAttackPayoff
  | (numBribed == numPlayers) = successfulAttackPayoff - bribe*(fromIntegral numBribed)
  | (otherwise)               = -bribe*(fromIntegral numBribed)
  where numPlayers = length bribesAccepted
        numBribed  = length (filter id bribesAccepted)

generateGame "completeGame" ["numPlayers", "reward", "costOfCapital", "maxBribe", "successfulAttackPayoff", "safeDepositProportion"] $ block [] []
  [line [ [| replicate numPlayers costOfCapital |] ] ["discard1"] [| population [depositStagePlayer ("Player " ++ show n) 0 10 0.1 0.001 | n <- [1 .. numPlayers]] |] ["deposits"] [ [| replicate numPlayers () |] ],
   line [ [| deposits |] ] [] [| dependentDecision "Attacker" (const [0, 0.0025 .. maxBribe]) |] ["bribe"] [ [| attackerPayoff bribesAccepted bribe successfulAttackPayoff |] ],
   line [ [| replicate numPlayers (deposits, bribe) |] ] ["discard2"] [| population [playingStagePlayer ("Player " ++ show n) [True, False] | n <- [1 .. numPlayers]] |] ["moves"] [ [| zip (payoffInt safeDepositProportion reward deposits (obfuscate moves)) bribesAccepted |] ],
   line [ [| moves |] ] [] [| fromFunctions (map not) id |] ["bribesAccepted"] []]
  [] []

testFullThing numPlayers reward costOfCapital = equilibrium (completeGame numPlayers reward costOfCapital 10 1000 0) void
-- with 10 players, reward = 5, costOfCapital = 0.046

deviationPenalty i reward deposits safeDepositProportion = ((payoffInt safeDepositProportion reward deposits numPlayers) !! i)
                                   - ((payoffInt safeDepositProportion reward deposits (numPlayers - 1)) !! i)
  where numPlayers = length deposits

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

-- semi-smart strategy accepts profitable bribes for coc=0.05 + params of test2player
-- models a successful attack - NE for deposit1=deposit2=0
test2Strategy' coc deposit1 deposit2 bribe =
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

{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CensorTests where

import Control.Arrow (Kleisli(..))
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
import OpenGames.Engine.Diagnostics
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax
import Numeric.Probability.Distribution (certainly, fromFreqs, uniform, T(..))
import Data.Ord (comparing)
import Data.List (maximumBy)

import Test.Hspec
import Test.QuickCheck

import Language.Haskell.TH.Syntax

data CensorMove = Honest | Censor deriving (Eq, Ord, Show)
data CarolObservation = Online | Offline deriving (Eq, Ord, Show)
type Deposit = Double

reward, offlinePenalty :: Double
reward = 3
offlinePenalty = 0

carolObservation ::(CensorMove, CensorMove, CensorMove) -> (CarolObservation, CarolObservation, CarolObservation)
carolObservation (Honest, Honest, _) = (Online, Online, Online)
carolObservation (Honest, _, Honest) = (Online, Online, Online)
carolObservation (_, Honest, Honest) = (Online, Online, Online)
carolObservation (x, y, z) = (f x, f y, f z)
  where f Honest = Offline
        f Censor = Online

payoffs :: Double -> (Deposit, Deposit, Deposit) -> (CarolObservation, CarolObservation, CarolObservation) -> (Double, Double, Double)
payoffs _ (x, y, z) (Online, Online, Online) = (reward*x/(x+y+z), reward*y/(x+y+z), reward*z/(x+y+z))
payoffs censorPenalty (x, y, z) (Offline, Online, Online) = (-offlinePenalty*x/(x+y+z), -censorPenalty*y/(x+y+z), -censorPenalty*z/(x+y+z))
payoffs censorPenalty (x, y, z) (Online, Offline, Online) = (-censorPenalty*x/(x+y+z), -offlinePenalty*y/(x+y+z), -censorPenalty*z/(x+y+z))
payoffs censorPenalty (x, y, z) (Online, Online, Offline) = (-censorPenalty*x/(x+y+z), -censorPenalty*y/(x+y+z), -offlinePenalty*z/(x+y+z))

-------------------------------------------------------------------------------------------------
-- Deposit Game
-------------------------------------------------------------------------------------------------
generateGame "depositGameTH" [] $ GBlock ["costOfCapital"] []
  [line [] [] [|dependentDecision "Dave"  (const [0.0 .. 10.0])|] ["daveStake"]  [[|-costOfCapital*daveStake|]],
   line [] [] [|dependentDecision "Erika" (const [0.0 .. 10.0])|] ["erikaStake"] [[|-costOfCapital*erikaStake|]],
   line [] [] [|dependentDecision "Frank" (const [0.0 .. 10.0])|] ["frankStake"] [[|-costOfCapital*frankStake|]]]
  ["daveStake", "erikaStake", "frankStake"] []

depositGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\costOfCapital -> (costOfCapital, ())) (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Dave" (const [0.0 .. 10.0]))))))) >>> (fromFunctions (\(costOfCapital, daveStake) -> (costOfCapital, daveStake)) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ((costOfCapital, daveStake, erikaStake, frankStake), -costOfCapital*daveStake))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(costOfCapital, daveStake) -> ((costOfCapital, daveStake), ())) (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Erika" (const [0.0 .. 10.0]))))))) >>> (fromFunctions (\((costOfCapital, daveStake), erikaStake) -> (costOfCapital, daveStake, erikaStake)) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ((costOfCapital, daveStake, erikaStake, frankStake), -costOfCapital*erikaStake)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(costOfCapital, daveStake, erikaStake) -> ((costOfCapital, daveStake, erikaStake), ())) (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Frank" (const [0.0 .. 10.0]))))))) >>> (fromFunctions (\((costOfCapital, daveStake, erikaStake), frankStake) -> (costOfCapital, daveStake, erikaStake, frankStake)) (\(costOfCapital, daveStake, erikaStake, frankStake) -> ((costOfCapital, daveStake, erikaStake, frankStake), -costOfCapital*frankStake))))))))) >>> (fromLens (\(costOfCapital, daveStake, erikaStake, frankStake) -> (daveStake, erikaStake, frankStake)) (curry (\((costOfCapital, daveStake, erikaStake, frankStake), ()) -> (costOfCapital, daveStake, erikaStake, frankStake)))))

-------------------------------------------------------------------------------------------------
-- Censor Game
-------------------------------------------------------------------------------------------------
generateGame "censorGameTH" ["censorPenalty"] $ GBlock ["daveStake", "erikaStake", "frankStake", "bribe"] []
  [line (map param ["daveStake", "bribe"]) [] [|dependentDecision "Dave" (const [Honest, Censor])|] ["daveMove"] [[|case daveMove of {Honest -> davePayoff; Censor -> davePayoff + bribe}|]],
   line [param "erikaStake"] [] [|dependentDecision "Erika" (const [Honest, Censor])|] ["erikaMove"] [param "erikaPayoff"],
   line [param "frankStake"] [] [|dependentDecision "Frank" (const [Honest, Censor])|] ["frankMove"] [param "frankPayoff"],
   line [[|(daveStake, erikaStake, frankStake)|], [|carolObservation (daveMove, erikaMove, frankMove)|]] [] [|fromFunctions (uncurry (payoffs censorPenalty)) id|] ["davePayoff", "erikaPayoff", "frankPayoff"] []]
  ["daveMove"] []

censorGame censorPenalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe) -> ((daveStake, erikaStake, frankStake, bribe), (daveStake, bribe))) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Dave" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe), daveMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), case daveMove of {Honest -> davePayoff; Censor -> davePayoff + bribe}))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove), erikaStake)) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Erika" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe, daveMove), erikaMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), erikaPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove), frankStake)) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "Frank" (const [Honest, Censor]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove), frankMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), frankPayoff)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove), ((daveStake, erikaStake, frankStake), carolObservation (daveMove, erikaMove, frankMove)))) (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (uncurry (payoffs censorPenalty)) id)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove), (davePayoff, erikaPayoff, frankPayoff)) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff)) (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> ((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff) -> daveMove) (curry (\((daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove, erikaMove, frankMove, davePayoff, erikaPayoff, frankPayoff)))))

generateGame "fullCensorGameTH" ["censorPenalty"] $ block [] []
  [line [[|0.05|]] [] [|depositGameTH|] ["daveStake", "erikaStake", "frankStake"] [],
   line (map param ["daveStake", "erikaStake", "frankStake"] ++ [[|0|]]) [] [|censorGameTH censorPenalty|] ["daveMove"] []]
  [] []

fullCensorGame censorPenalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake, daveMove) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), 0.05)) (\((daveStake, erikaStake, frankStake, daveMove), ()) -> (daveStake, erikaStake, frankStake, daveMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((depositGame)))))) >>> (fromFunctions (\((), (daveStake, erikaStake, frankStake)) -> (daveStake, erikaStake, frankStake)) (\(daveStake, erikaStake, frankStake, daveMove) -> ((daveStake, erikaStake, frankStake, daveMove), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), (daveStake, erikaStake, frankStake, 0))) (\((daveStake, erikaStake, frankStake, daveMove), ()) -> (daveStake, erikaStake, frankStake, daveMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((censorGame censorPenalty)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake), daveMove) -> (daveStake, erikaStake, frankStake, daveMove)) (\(daveStake, erikaStake, frankStake, daveMove) -> ((daveStake, erikaStake, frankStake, daveMove), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake, daveMove) -> ()) (curry (\((daveStake, erikaStake, frankStake, daveMove), ()) -> (daveStake, erikaStake, frankStake, daveMove)))))

-------------------------------------------------------------------------------------------------
-- Censor Bribe
-------------------------------------------------------------------------------------------------

generateGame "bribeGameTH" [] $ block ["target", "move", "bribe"] []
  [line [param "target", [|()|]] [] [|roleDecision [()]|] ["dummy"] [[|case move of {Honest -> 0; Censor -> bribe}|]]]
  [] []

bribeGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(target, move, bribe, dummy) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(target, move, bribe) -> ((target, move, bribe), (target, ()))) (\((target, move, bribe, dummy), ()) -> (target, move, bribe, dummy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [()])))))) >>> (fromFunctions (\((target, move, bribe), dummy) -> (target, move, bribe, dummy)) (\(target, move, bribe, dummy) -> ((target, move, bribe, dummy), case move of {Honest -> 0; Censor -> bribe})))))))) >>> (fromLens (\(target, move, bribe, dummy) -> ()) (curry (\((target, move, bribe, dummy), ()) -> (target, move, bribe, dummy)))))

generateGame "censorBribeGameTH" ["censorPenalty", "bribeBudget"]
  [line [[|0.05|]] [] [|depositGame|] ["daveStake", "erikaStake", "frankStake"] [],
   line [] [] [|nature (uniform [0, bribeBudget])|] ["bribe"] [],
   line (map param ["daveStake", "erikaStake", "frankStake", "bribe"]) [] [|censorGame censorPenalty|] ["daveMove"] []]

censorBribeGame censorPenalty bribeBudget = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), 0.05)) (\((daveStake, erikaStake, frankStake, bribe, daveMove), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((depositGame)))))) >>> (fromFunctions (\((), (daveStake, erikaStake, frankStake)) -> (daveStake, erikaStake, frankStake)) (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake) -> ((daveStake, erikaStake, frankStake), ())) (\((daveStake, erikaStake, frankStake, bribe, daveMove), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0, bribeBudget]))))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake), bribe) -> (daveStake, erikaStake, frankStake, bribe)) (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(daveStake, erikaStake, frankStake, bribe) -> ((daveStake, erikaStake, frankStake, bribe), (daveStake, erikaStake, frankStake, bribe))) (\((daveStake, erikaStake, frankStake, bribe, daveMove), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((censorGame censorPenalty)))))) >>> (fromFunctions (\((daveStake, erikaStake, frankStake, bribe), daveMove) -> (daveStake, erikaStake, frankStake, bribe, daveMove)) (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ((daveStake, erikaStake, frankStake, bribe, daveMove), ()))))))))) >>> (fromLens (\(daveStake, erikaStake, frankStake, bribe, daveMove) -> ()) (curry (\((daveStake, erikaStake, frankStake, bribe, daveMove), ()) -> (daveStake, erikaStake, frankStake, bribe, daveMove)))))


-------------------------------------------------------------------------------------------------
-- Game operations
-------------------------------------------------------------------------------------------------

censorBribeGameEq censorPenalty bribeBudget a b c d e f =
  equilibrium (censorBribeGame censorPenalty bribeBudget)
              void
              ( (Kleisli $ const a, Kleisli $ const b, Kleisli $ const c)
              , ()
              , (Kleisli d, Kleisli e, Kleisli f, ()))

censorStrategy censorPenalty = ((Kleisli $ const $ certainly 10,
                   Kleisli $ const $ certainly 10,
                   Kleisli $ const $ certainly 10),
                   (),
                  (Kleisli $ \(_, bribe) -> let davePayoff daveMove = let (x, _, _) = payoffs censorPenalty (10, 10, 10) (carolObservation (daveMove, Censor, Honest)) in x
                                             in if bribe + davePayoff Censor <= davePayoff Honest then certainly Honest else certainly Censor,
                   Kleisli $ const $ certainly Censor,
                   Kleisli $ const $ certainly Honest,
                   ()))

minimumBribeBudget game censorPenalty = minimum [bribeBudget | bribeBudget <- [0, 0.1 .. 10], not $ null (equilibrium (game censorPenalty bribeBudget) void (censorStrategy censorPenalty))]

penaltiesWithMinimumBudget :: (Double
     -> Double
     -> OpticOpenGame
          StochasticStatefulOptic
          StochasticStatefulContext
          [OpenGames.Engine.Diagnostics.DiagnosticInfo]
          ((Kleisli Stochastic () Double, Kleisli Stochastic () Double,
            Kleisli Stochastic () Double),
           (),
           (Kleisli Stochastic (Deposit, Double) CensorMove,
            Kleisli Stochastic Deposit CensorMove,
            Kleisli Stochastic Deposit CensorMove, ()))
          ()
          ()
          ()
          ()) -> [(Double, Double)]
penaltiesWithMinimumBudget game = [(censorPenalty, minimumBribeBudget censorBribeGame censorPenalty) | censorPenalty <- [0, 0.01 .. 1]]

minimumCensorPenaltyWithMinmaxBudget :: (Double, Double)
minimumCensorPenaltyWithMinmaxBudget =
  minimum (Prelude.filter (\(x,y) -> y == minmaxBudget)
          (penaltiesWithMinimumBudget censorBribeGame))
    where minmaxBudget = snd (maximumBy (comparing snd) (penaltiesWithMinimumBudget censorBribeGame))

instance Arbitrary CensorMove where
  arbitrary = elements [Censor, Honest]
  shrink Honest = [Censor]
  shrink Censor = [Honest]

instance (Arbitrary t) => Arbitrary (T Double t) where
  arbitrary = certainly <$> arbitrary

testCensor :: IO ()
testCensor = hspec $ do
  describe "testing compatibility between old and new censor games" $ do
    it "should find the same minimum bribe" $ property $
      \penalty budget ->
        equilibrium (censorBribeGame penalty budget) void (censorStrategy penalty)
        ==
       equilibrium (censorBribeGameTH penalty budget) void (censorStrategy penalty)
    it "full censorGameEquilibrium" $ property $
      \penalty
       (Fn (a :: () -> T Double Double))
       (Fn (b :: () -> T Double Double))
       (Fn (c :: () -> T Double Double))
       (Fn (d :: (Deposit, Double) -> T Double CensorMove))
       (Fn (e :: Deposit -> T Double CensorMove))
       (Fn (f :: Deposit -> T Double CensorMove)) ->
         equilibrium (fullCensorGame penalty)
                     void
                     ( (Kleisli a, Kleisli b, Kleisli c)
                     , (Kleisli d, Kleisli e, Kleisli f, ()))
         ==
         equilibrium (fullCensorGameTH penalty)
                     void
                     ( (Kleisli a, Kleisli b, Kleisli c)
                     , (Kleisli d, Kleisli e, Kleisli f, ()))




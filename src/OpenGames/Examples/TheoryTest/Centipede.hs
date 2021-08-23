{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

module OpenGames.Examples.TheoryTest.Centipede where

import           Control.Arrow (Kleisli(..))
import           Numeric.Probability.Distribution

import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.Types
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Compile
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
import Language.Haskell.TH


-- Test of operators NE vs SPNE

data Action = Take | Wait
  deriving (Ord,Eq,Show)

payoff :: [Action] -> Double -> Action -> Double
payoff ls payoff Take = if all (Wait ==) ls then payoff else 0
payoff ls _      Wait = 0

-- One step interaction between 2 players
generateGame "centipedeStepRolesState" []
  (Block ["payoffSender","payoffReceiver","sender","receiver","pastDecs"] []
  [Line Nothing [ [|sender|], [|payoffSender |]]   [] [|roleDecision   [Take,Wait]|] ["dec"] [[|payoff pastDecs payoffSender dec|]]
  ,Line Nothing [ [|receiver|],[|payoffReceiver|]] [] [|roleDecision [True]|]   ["ignore"]   [[|payoff pastDecs payoffReceiver dec|]]]
  [[|payoffSender|],[|payoffReceiver|],[|dec:pastDecs|]] [] :: Block String (Q Exp))


-- Centipede game with 4 iterations
generateGame "centipede4" [] (
  Block [] []
  [Line Nothing [[|1|], [|0|], [|"player1"|], [|"player2"|],[|[Wait]|]] [] [|centipedeStepRolesState|] ["payoffSenderR1", "payoffReceiverR1","decs1"] []
  ,Line Nothing [[|payoffSenderR1+1|],[|payoffReceiverR1|],[|"player2"|],[|"player1"|],[|decs1|]] [] [|centipedeStepRolesState|] ["payoffSenderR2", "payoffReceiverR2","decs2"] []
  ,Line Nothing [[|payoffSenderR2+1|],[|payoffReceiverR1+1|],[|"player1"|],[|"player2"|],[|decs2|]] [] [|centipedeStepRolesState|] ["payoffSenderR3", "payoffReceiverR3","decs3"] []
  ,Line Nothing [[|payoffSenderR3+1|],[|payoffReceiverR3+1|],[|"player2"|],[|"player1"|],[|decs3|]] [] [|centipedeStepRolesState|] ["payoffSenderR4", "payoffReceiverR4","decs4"] []]
  [] [] :: Block String (Q Exp))


testCentipede = equilibrium centipede4 void


-- strategy profil which is SP
testStrategy4SPNE
  :: ((Kleisli Stochastic b1 Action, Kleisli Stochastic b2 Bool),
      (Kleisli Stochastic b3 Action, Kleisli Stochastic b4 Bool),
      (Kleisli Stochastic b5 Action, Kleisli Stochastic b6 Bool),
      (Kleisli Stochastic b5 Action, Kleisli Stochastic b6 Bool))
testStrategy4SPNE = ((Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True), (Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True), (Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True), (Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True))


-- strategy profil which is Nash but not SP
testStrategy4NE
  :: ((Kleisli Stochastic b1 Action, Kleisli Stochastic b2 Bool),
      (Kleisli Stochastic b3 Action, Kleisli Stochastic b4 Bool),
      (Kleisli Stochastic b5 Action, Kleisli Stochastic b6 Bool),
      (Kleisli Stochastic b5 Action, Kleisli Stochastic b6 Bool))
testStrategy4NE = ((Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True), (Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True), (Kleisli $ const $ certainly Wait, Kleisli $ const $ certainly True), (Kleisli $ const $ certainly Take, Kleisli $ const $ certainly True))



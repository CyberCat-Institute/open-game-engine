{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.Consensus.TestPayoffs where

import Data.HashMap (Map)

import Control.Monad.State
import Numeric.Probability.Distribution (T(..), certainly, uniform, fromFreqs)

import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Compile
import OpenGames.Engine.Diagnostics
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision

import Control.Arrow (Kleisli(..))

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix Cooperate Cooperate   = 3
prisonersDilemmaMatrix Cooperate Defect  = 0
prisonersDilemmaMatrix Defect Cooperate  = 5
prisonersDilemmaMatrix Defect Defect = 1



prisonersDilemma2 = [opengame|

   inputs    :  ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1' ;
   returns   : prisonersDilemmaMatrix decisionPlayer1' decisionPlayer2' ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2' ;
   returns   : prisonersDilemmaMatrix decisionPlayer2' decisionPlayer1' ;


   :----------------------------:

   outputs   :  ;
   returns   :   ;
  |]

testEq = equilibrium prisonersDilemma2 OpenGames.Engine.OpticClass.void

testCooperate :: Kleisli Stochastic () ActionPD
testCooperate = Kleisli $ const $ certainly Cooperate

testStrategy = ( testCooperate
               , testCooperate
               , testCooperate
               , testCooperate)

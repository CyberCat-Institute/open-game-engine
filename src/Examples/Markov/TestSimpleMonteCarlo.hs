{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- TODO 0 Benchmark against alternative implementation is there a speed update?

module Examples.Markov.TestSimpleMonteCarlo where

import           Engine.Engine
import           Preprocessor.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..),prisonersDilemmaMatrix)
import           Examples.Markov.TestSimpleMonteCarlo.Continuation

import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST

import qualified Data.Vector as V
import           Debug.Trace
import System.Random.MWC.CondensedTable
import System.Random
import System.Random.Stateful
import Numeric.Probability.Distribution hiding (map, lift, filter)

prisonersDilemma  :: OpenGame
                              StochasticStatefulOptic
                              StochasticStatefulContext
                              ('[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                                 Kleisli Stochastic (ActionPD, ActionPD) ActionPD])
                              ('[[DiagnosticInfoBayesian (ActionPD, ActionPD) ActionPD],
                                 [DiagnosticInfoBayesian (ActionPD, ActionPD) ActionPD]])
                              (ActionPD, ActionPD)
                              ()
                              (ActionPD, ActionPD)
                              ()
discountFactor = 1

prisonersDilemma = [opengame|

   inputs    : (dec1Old,dec2Old) ;
   feedback  :      ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   :      ;
  |]



-- Add strategy for stage game
stageStrategy :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategy = Kleisli $
   (\case
       (Cooperate,Cooperate) -> playDeterministically Cooperate
       (_,_)         -> playDeterministically Defect)
-- Stage strategy tuple
strategyTuple = stageStrategy ::- stageStrategy ::- Nil

-- Testing for stoch behavior and slow down
stageStrategyTest :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategyTest = Kleisli $ const $ uniformDist [Cooperate,Defect]
-- Stage strategy tuple
strategyTupleTest = stageStrategy ::- stageStrategy ::- Nil



-- fix context used for the evaluation
contextCont sampleSize iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> sampleDetermineContinuationPayoffsStoch sampleSize iterator strat action)



repeatedPDEq sampleSize iterator strat initialAction = evaluate prisonersDilemma strat context
  where context  = contextCont sampleSize iterator strat initialAction

eqOutput sampleSize iterator strat initialAction = generateOutput $ repeatedPDEq sampleSize iterator strat initialAction

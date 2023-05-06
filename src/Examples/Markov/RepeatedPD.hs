{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Markov.RepeatedPD where

import Control.Monad.State hiding (lift, state, void)
import qualified Control.Monad.State as ST
import Debug.Trace
import Examples.SimultaneousMoves (ActionPD (..), prisonersDilemmaMatrix)
import Numeric.Probability.Distribution hiding (filter, lift, map)
import OpenGames
import OpenGames.Preprocessor

prisonersDilemma ::
  OpenGame
    StochasticStatefulOptic
    StochasticStatefulContext
    ( '[ Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
         Kleisli Stochastic (ActionPD, ActionPD) ActionPD
       ]
    )
    ( '[ [DiagnosticInfoBayesian (ActionPD, ActionPD) ActionPD],
         [DiagnosticInfoBayesian (ActionPD, ActionPD) ActionPD]
       ]
    )
    (ActionPD, ActionPD)
    ()
    (ActionPD, ActionPD)
    ()

discountFactor = 0.5

prisonersDilemma =
  [opengame|

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
stageStrategy =
  Kleisli $
    ( \case
        (Cooperate, Cooperate) -> playDeterministically Cooperate
        (_, _) -> playDeterministically Defect
    )

-- Stage strategy tuple
strategyTuple = stageStrategy :- stageStrategy :- Nil

-- Testing for stoch behavior and slow down
stageStrategyTest :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategyTest = Kleisli $ const $ distFromList [(Cooperate, 0.9), (Defect, 0.1)]

-- Stage strategy tuple
strategyTupleTest = stageStrategyTest :- stageStrategyTest :- Nil

-- extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z, a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: StochasticStatefulOptic s () a () -> s -> Stochastic a
extractNextState (StochasticStatefulOptic v _) x = do
  (z, a) <- v x
  pure a

executeStrat strat = play prisonersDilemma strat

-- determine continuation for iterator, with the same repeated strategy
determineContinuationPayoffs ::
  Integer ->
  List
    '[ Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
       Kleisli Stochastic (ActionPD, ActionPD) ActionPD
     ] ->
  (ActionPD, ActionPD) ->
  StateT Vector Stochastic ()
determineContinuationPayoffs 1 strat action = pure ()
determineContinuationPayoffs iterator strat action = do
  extractContinuation executeStrat action
  nextInput <- ST.lift $ extractNextState executeStrat action
  determineContinuationPayoffs (pred iterator) strat nextInput
  where
    executeStrat = play prisonersDilemma strat

-- fix context used for the evaluation
contextCont iterator strat initialAction = StochasticStatefulContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action)

repeatedPDEq iterator strat initialAction = evaluate prisonersDilemma strat context
  where
    context = contextCont iterator strat initialAction

eqOutput iterator strat initialAction = generateIsEq $ repeatedPDEq iterator strat initialAction

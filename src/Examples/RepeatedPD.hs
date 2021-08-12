{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.RepeatedPD where 


import           Engine.Engine
import           Preprocessor.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..),prisonersDilemmaMatrix)

import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST

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
stageStrategy1,stageStrategy2 :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategy1 = Kleisli $
   (\case
       (Cooperate,Cooperate) -> playDeterministically Cooperate
       (_,_)         -> playDeterministically Defect)
stageStrategy2 = Kleisli $
   (\case
       (Cooperate,Cooperate) -> playDeterministically Cooperate
       (_,_)         -> playDeterministically Defect)

-- Stage strategy tuple
strategyTuple = stageStrategy1 ::- stageStrategy2 ::- Nil
strategyTuple2 = stageStrategy1 ::- stageStrategy2 ::- stageStrategy1 ::- stageStrategy2 ::- Nil
strategyTuple3 = stageStrategy1 ::- stageStrategy2 ::- stageStrategy1 ::- stageStrategy2 ::- stageStrategy1 ::- stageStrategy2 ::- Nil

-- initial context
initialContext :: StochasticStatefulContext (ActionPD, ActionPD) () (ActionPD, ActionPD) ()
initialContext = StochasticStatefulContext (pure ((),(Cooperate,Cooperate))) (\_ _ -> return ())

-- stageGameEquilibrium
stageGameEq = evaluate prisonersDilemma strategyTuple initialContext


-- extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()



extractNextState :: StochasticStatefulOptic s () a () -> s -> Stochastic a
extractNextState (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a


extractContinuation' :: StochasticStatefulOptic s () a () -> Stochastic s -> StateT Vector Stochastic ()
extractContinuation' (StochasticStatefulOptic v u) x = do
  x' <- ST.lift x
  (z,a) <- ST.lift (v x')
  u z ()

extractNextState' :: StochasticStatefulOptic s () a () -> Stochastic s -> Stochastic a
extractNextState' (StochasticStatefulOptic v _) x = do
  x' <-  x
  (z,a) <- v x'
  pure a


-- determine continuation for iterator, with the same repeated strategy

determineContinuationPayoffs input 1        = extractContinuation (play prisonersDilemma strategyTuple) input
determineContinuationPayoffs input iterator = do
   extractContinuation executeStrat input
   nextInput <- ST.lift $ extractNextState executeStrat input
   determineContinuationPayoffs nextInput (pred iterator)
 where executeStrat =  play prisonersDilemma strategyTuple


determineContinuationPayoffs' input 1        = extractContinuation' (play prisonersDilemma strategyTuple) input
determineContinuationPayoffs' input iterator = do
 --  extractContinuation' executeStrat input
   let nextInput = extractNextState' executeStrat input
   determineContinuationPayoffs' nextInput (pred iterator)
 where executeStrat =  play prisonersDilemma strategyTuple



repeatedPDEq iterator initialCondition = evaluate prisonersDilemma strategyTuple context
  where context      = StochasticStatefulContext (pure ((),initialCondition)) (\_ _ -> determineContinuationPayoffs initialCondition iterator)


repeatedPDEq' iterator initialState = evaluate prisonersDilemma strategyTuple context
  where executeStrat = play prisonersDilemma strategyTuple
        nextState    = extractNextState' executeStrat (pure initialState)
        context      = StochasticStatefulContext (pure ((),initialState)) (\_ _ -> determineContinuationPayoffs' nextState iterator)

repeatedPDEq'' iterator initialState = evaluate prisonersDilemma strategyTuple context
  where executeStrat = play prisonersDilemma strategyTuple
        nextState strat   = extractNextState' executeStrat (pure strat)
        context      = StochasticStatefulContext (pure ((),initialState)) (\_ strat -> determineContinuationPayoffs' (nextState strat) iterator)



eqOutput iterator initialCondition = generateIsEq $ repeatedPDEq iterator initialCondition

eqOutput' iterator initialCondition = generateOutput $ repeatedPDEq' iterator initialCondition

eqOutput'' iterator initialCondition = generateOutput $ repeatedPDEq'' iterator initialCondition

-- The problem seems to be that a unilateral deviation in the stage game does not affect the continuation payoff. 


-- test an extreme assumption on the context
{-
testContext :: (ActionPD,ActionPD) -> StochasticStatefulContext (ActionPD,ActionPD) () ActionPD ()
testContext initialCondition =
  StochasticStatefulContext
    (pure ((),initialCondition))
    (\_ -> (\case
              Cooperate -> 1000
              Defect    -> 0))





prisonersDilemma2 = [opengame|

   inputs    : (dec1Old,dec2Old) ;
   feedback  :      ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 + return1 ;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 + return2 ;

   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   : (return1,return2)     ;
  |]



initialContext2 :: StochasticStatefulContext (ActionPD, ActionPD) () (ActionPD, ActionPD) (Double,Double)
initialContext2 =
  StochasticStatefulContext
     (pure ((),(Cooperate,Cooperate)))
     (\_ -> (\case
             {(Cooperate,Cooperate) -> return (100,100);
              (_        ,_        ) -> return (0,0)}))


--initialContext3 :: StochasticStatefulContext (ActionPD, ActionPD) () (ActionPD, ActionPD) (Double,Double)
initialContext3 initialState iterator = evaluate prisonersDilemma2 strategyTuple context 
  where executeStrat = play prisonersDilemma strategyTuple
        nextState    = extractNextState' executeStrat (pure initialState)
        context      = StochasticStatefulContext (pure ((),initialState)) (\_ _ -> determineContinuationPayoffs' nextState iterator)

  

testEq = generateOutput $ evaluate prisonersDilemma2 strategyTuple initialContext
-}

-- For a an optic (derived from a play for a given strategy), and state (which is also the action), derive a new StateT
extractContinuation2 :: StochasticStatefulOptic s () a () -> Stochastic s -> StateT Vector Stochastic ()
extractContinuation2 (StochasticStatefulOptic v u) x = do
  x' <- ST.lift x
  (z,a) <- ST.lift (v x')
  u z ()

-- For a an optic (derived from a play for a given strategy), and state derive the action which was played, which in turn is then the next state
extractNextState2 :: StochasticStatefulOptic s () a () -> Stochastic s -> Stochastic a
extractNextState2 (StochasticStatefulOptic v _) x = do
  x' <-  x
  (z,a) <- v x'
  pure a

-- For a an optic (derived from a play for a given strategy), and state (which is also the action), derive a new StateT
extractContinuation3 :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation3 (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- For a an optic (derived from a play for a given strategy), and state derive the action which was played, which in turn is then the next state
extractNextState3 :: StochasticStatefulOptic s () a () -> s -> Stochastic a
extractNextState3 (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a




-- What is that payoff? Given a strategy and an action evaluate how the rounds will play out.
continuationPayoffs
  :: (Eq t, Num t, Enum t) =>
     t
     -> List
          '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
            Kleisli Stochastic (ActionPD, ActionPD) ActionPD]
     -> (ActionPD, ActionPD)
     -> StateT Vector Stochastic ()
continuationPayoffs iterator strat action
  | iterator == 1 = extractContinuation3 (execute strat) action
  | otherwise     = do
      extractContinuation3 (execute strat) action
      actionNew <- ST.lift $ nextState strat action
      continuationPayoffs (pred iterator) strat actionNew
  where execute strat'           = play prisonersDilemma strat' -- results in an optic
        nextState strat' action' = extractNextState3 (execute strat') action'


-- Gives the context with the initial condition, the state, and for an action taken determines the continuation payoff
contextCont
  :: (Eq t1, Num t1, Enum t1) =>
     t1
     -> List
          '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
            Kleisli Stochastic (ActionPD, ActionPD) ActionPD]
     -> (ActionPD, ActionPD)
     -> StochasticStatefulContext
          (ActionPD, ActionPD) () (ActionPD, ActionPD) ()
contextCont iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> continuationPayoffs iterator strat action)
  



-- evaluate the one stage game with a given strategy and a given initial state, as context, use the payoff derived from continuously playing that stage game
evaluateIteratedPD iterator strat initialAction = generateOutput $ evaluate prisonersDilemma strat context
  where context = contextCont iterator strat initialAction



{-

repeatedPDEq3 iterator initialState = evaluate prisonersDilemma strategyTuple context
  where executeStrat = play prisonersDilemma strategyTuple
        nextState strat   = extractNextState' executeStrat (pure strat)
        context      = StochasticStatefulContext (pure ((),initialState)) (\_ strat -> determineContinuationPayoffs' (nextState strat) iterator)


-}

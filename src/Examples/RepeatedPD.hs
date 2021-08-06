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
discountFactor = 0.95

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
       (Cooperate,_) -> playDeterministically Cooperate
       (_,_)         -> playDeterministically Defect)
stageStrategy2 = Kleisli $
   (\case
       (_,Cooperate) -> playDeterministically Cooperate
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
  

-- TODO Iterator on the type level needs to be added

iteratedPD  = (prisonersDilemma >>> prisonersDilemma) 

iteratedPD3 = iteratedPD >>> prisonersDilemma 

iteratedPD4 = iteratedPD >>> iteratedPD


iteratedPD8 = iteratedPD4 >>> iteratedPD4


-- equilibrium of complete game
-- TODO this is wrong due to the fixed elements
repeatedPDEq = evaluate prisonersDilemma strategyTuple context
  where context      = StochasticStatefulContext (pure ((),(Cooperate,Cooperate))) (\_ _ -> continuation (Cooperate,Cooperate))
        continuation = extractContinuation (play iteratedPD3 strategyTuple3)

eqOutput = generateIsEq $ repeatedPDEq


test = extractContinuation (play iteratedPD strategyTuple2) (Cooperate, Cooperate)


-- testing the state payoffs are correct
prisonersDilemma2 = [opengame|

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

   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1' ;
   returns   : prisonersDilemmaMatrix decisionPlayer1' decisionPlayer2' ;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2' ;
   returns   : prisonersDilemmaMatrix decisionPlayer2' decisionPlayer1' ;


   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   :      ;
  |]


stageGameEq2 = generateIsEq $ evaluate prisonersDilemma2 strategyTuple2 initialContext




prisonersDilemmaBasic = [opengame|

   inputs    :  ;
   feedback  :  ;

   :----------------------------:
   inputs    :   ;
   feedback  :   ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :    ;
   feedback  :    ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]


prisonersDilemmaComposed = [opengame|

   inputs    :  ;
   feedback  :  ;

   :----------------------------:
   inputs    :   ;
   feedback  :   ;
   operation : prisonersDilemmaBasic ;
   outputs   : ;
   returns   : ;

   inputs    :    ;
   feedback  :    ;
   operation : prisonersDilemmaBasic ;
   outputs   : ;
   returns   : ;


   :----------------------------:

   outputs   :  ;
   returns   :  ;

   |]


prisonersDilemmaComposed2 = prisonersDilemmaBasic >>> prisonersDilemmaBasic

composedEq strat =generateIsEq $  evaluate prisonersDilemmaComposed2 strat void

stratSimple =  Kleisli $ const $ playDeterministically Cooperate

stratTupleSimple = stratSimple ::- stratSimple ::- stratSimple ::- stratSimple ::- Nil

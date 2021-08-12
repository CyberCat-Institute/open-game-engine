{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.TwoStageMarkov where 

import           Data.Tuple.Extra (uncurry3)
import           Engine.Engine
import           Preprocessor.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..))

import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST

import Numeric.Probability.Distribution hiding (map, lift, filter)

type EndState = Bool

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: EndState -> ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix True _         _           = 0
prisonersDilemmaMatrix _    Cooperate Cooperate   = 3
prisonersDilemmaMatrix _    Cooperate Defect      = 0
prisonersDilemmaMatrix _    Defect Cooperate      = 5
prisonersDilemmaMatrix _    Defect Defect         = 1

-- once in endstate, we stay there
-- the transition happens with 0.5 probability if one of the actions is defect
transitionEndState :: EndState -> ActionPD -> ActionPD -> Stochastic EndState
transitionEndState True _ _ = playDeterministically True
transitionEndState False Defect _ = uniform [True,False]
transitionEndState False _      Defect = uniform [True,False]
transitionEndState False Cooperate Cooperate = playDeterministically False 


prisonersDilemma  :: OpenGame
                              StochasticStatefulOptic
                              StochasticStatefulContext
                              ('[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                                 Kleisli Stochastic (ActionPD, ActionPD) ActionPD])
                              ('[[DiagnosticInfoBayesian (ActionPD, ActionPD) ActionPD],
                                 [DiagnosticInfoBayesian (ActionPD, ActionPD) ActionPD]])
                              (ActionPD, ActionPD, EndState)
                              ()
                              (ActionPD, ActionPD)
                              ()
discountFactor = 1

punishment True  = - 10
punishment False =  0

prisonersDilemma = [opengame|

   inputs    : (dec1Old,dec2Old,endState) ;
   feedback  :      ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix endState decisionPlayer1 decisionPlayer2 ;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix endState decisionPlayer2 decisionPlayer1 ;


   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   :      ;
  |]

endState = [opengame|

   inputs    : gameState ;
   feedback  :  ;

   :----------------------------:
   inputs    : ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Defect,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : punishment gameState;

   inputs    :   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Defect,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : punishment gameState ;

   
   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]



completeGame = [opengame|

   inputs    : (dec1Old,dec2Old,gameStateOld) ;
   feedback  :  ;

   :----------------------------:
   inputs    : (dec1Old,dec2Old,gameStateOld);
   feedback  :      ;
   operation : prisonersDilemma ;
   outputs   : (dec1New,dec2New) ;
   returns   : ;

   inputs    :  gameStateOld ;
   feedback  :      ;
   operation : endState ;
   outputs   : ;
   returns   : ;

   inputs    :  (gameStateOld,dec1New,dec2New) ;
   feedback  :      ;
   operation : liftStochasticForward $ uncurry3 transitionEndState;
   outputs   : gameStateNew;
   returns   : ;



   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;


   :----------------------------:

   outputs   :  (dec1New,dec2New,gameStateNew)  ;
   returns   :         ;
  |]


  

-- Add strategy for stage game 
stageStrategy :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategy = Kleisli $
   (\case
       (Cooperate,Cooperate) -> playDeterministically Cooperate
       (_,_)         -> playDeterministically Defect)
stageStrategy2 :: Kleisli Stochastic () ActionPD
stageStrategy2 = Kleisli $ const $ playDeterministically Defect

-- Stage strategy tuple
strategyTuple = stageStrategy ::- stageStrategy ::- stageStrategy2 ::- stageStrategy2 ::-  Nil


-- extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: StochasticStatefulOptic s () (ActionPD,ActionPD,EndState) () -> s -> Stochastic (ActionPD,ActionPD,EndState)
extractNextState (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a



-- determine continuation for iterator, with the same repeated strategy
determineContinuationPayoffs :: Integer
                             ->  List
                                    '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                                      Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                                      Kleisli Stochastic () ActionPD, Kleisli Stochastic () ActionPD]
                             -> (ActionPD,ActionPD,EndState)
                             -> StateT Vector Stochastic ()
determineContinuationPayoffs 1        strat action = pure ()
determineContinuationPayoffs iterator strat action = do
   extractContinuation executeStrat action
   nextInput <- ST.lift $ extractNextState executeStrat action
   determineContinuationPayoffs (pred iterator) strat nextInput
 where executeStrat =  play completeGame strat


-- fix context used for the evaluation
contextCont iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action)



repeatedCompleteGameEq iterator strat initialAction = evaluate completeGame strat context
  where context  = contextCont iterator strat initialAction





eqOutput iterator strat initialAction = generateIsEq $ repeatedCompleteGameEq iterator strat initialAction


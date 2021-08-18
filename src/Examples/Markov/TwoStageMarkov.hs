{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.Markov.TwoStageMarkov where 

import           Data.Tuple.Extra (uncurry3)
import           Engine.Engine
import           Preprocessor.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..), Location(..))

import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST

import Numeric.Probability.Distribution hiding (map, lift, filter)


-- TODO: Add stage game with proper backtransitioning 


--------
-- Types

type EndState = Bool

----------
-- Payoffs

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: EndState -> ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix True _         _           = 0
prisonersDilemmaMatrix _    Cooperate Cooperate   = 3
prisonersDilemmaMatrix _    Cooperate Defect      = 0
prisonersDilemmaMatrix _    Defect Cooperate      = 5
prisonersDilemmaMatrix _    Defect Defect         = 1


-- | Payoff matrix meeting in NY
meetingInNYMatrix :: EndState -> Location -> Location -> Double
meetingInNYMatrix False _ _ = 0
meetingInNYMatrix True  x y = if x == y then 1 else 0

discountFactor = 0.6

-- | Fixed payoff for single decision end state
punishment True  = - 10
punishment False =  0



----------------------
-- Auxiliary functions

-- Once in endstate, we stay there
-- The transition happens with 0.5 probability if one of the actions is defect
transitionEndState :: EndState -> ActionPD -> ActionPD -> Stochastic EndState
transitionEndState True _ _ = playDeterministically True
transitionEndState False Defect _ = uniform [True,False]
transitionEndState False _      Defect = uniform [True,False]
transitionEndState False Cooperate Cooperate = playDeterministically False 

-------------
-- Open games

-- The baseline stage game: prisoner's dilemma
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

-- absorbing state
-- only one action and therefore deterministic payoff
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

-- define a proper stage game, here MeetingInNY, where we end up
endStateGame = [opengame|

   inputs    : gameState ;
   feedback  :  ;

   :----------------------------:

   inputs    : ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [EmpireState,GrandCentral]);
   outputs   : decisionPlayer1 ;
   returns   : meetingInNYMatrix gameState decisionPlayer1 decisionPlayer2 ;


   inputs    : ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [EmpireState,GrandCentral]);
   outputs   : decisionPlayer2 ;
   returns   : meetingInNYMatrix gameState decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]


-- define the whole game, here with pathological endgame
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

-- define the whole game, here with pathological endgame
completeGameMNY = [opengame|

   inputs    : (dec1Old,dec2Old,gameStateOld) ;
   feedback  :  ;

   :----------------------------:
   inputs    : (dec1Old,dec2Old,gameStateOld);
   feedback  :      ;
   operation : prisonersDilemma ;
   outputs   : (dec1New,dec2New) ;
   returns   : ;

   inputs    : gameStateOld ;
   feedback  :      ;
   operation : endStateGame ;
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


-------------
-- Strategies

-- Add strategy for stage game 
stageStrategy :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategy = Kleisli $
   (\case
       (Cooperate,Cooperate) -> playDeterministically Cooperate
       (_,_)         -> playDeterministically Defect)

-- Strategy for pathological end stage game
endStageStrategy :: Kleisli Stochastic () ActionPD
endStageStrategy = Kleisli $ const $ playDeterministically Defect

-- Strategy for end stage game, meeting in NY
endStageStrategyMNY :: Kleisli Stochastic () Location
endStageStrategyMNY = Kleisli $ const $ playDeterministically EmpireState

-- Strategy tuple for complete game
strategyTuple = stageStrategy ::- stageStrategy ::- endStageStrategy ::- endStageStrategy ::-  Nil

-- Strategy tuple for complete game with meeting in NY
strategyTupleMNY = stageStrategy ::- stageStrategy ::- endStageStrategyMNY ::- endStageStrategyMNY ::-  Nil


-----------------------
-- Continuation payoffs

-- Extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- Extract next state (action)
extractNextState :: StochasticStatefulOptic s () (a,a,EndState) () -> s -> Stochastic (a,a,EndState)
extractNextState (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a


-- Determine continuation for iterator, with the same repeated strategy, using the pathological endgame
determineContinuationPayoffsMNY 1        strat action = pure ()
determineContinuationPayoffsMNY iterator strat action = do
   extractContinuation executeStrat action
   nextInput <- ST.lift $ extractNextState executeStrat action
   determineContinuationPayoffsMNY (pred iterator) strat nextInput
 where executeStrat =  play completeGameMNY strat


-- Determine continuation for iterator, with the same repeated strategy, using the pathological endgame
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

----------
-- Context

-- Context used for the evaluation of the pathological end state
contextCont iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action)

-- Context used for the evaluation of the pathological end state
contextContMNY iterator strat initialAction = StochasticStatefulContext (pure ((),initialAction)) (\_ action -> determineContinuationPayoffsMNY iterator strat action)



--------------
-- Equilibrium

-- Pathological end state
repeatedCompleteGameEq iterator strat initialAction = evaluate completeGame strat context
  where context  = contextCont iterator strat initialAction

-- Meeting in NY endgame
repeatedCompleteGameMNYEq iterator strat initialAction = evaluate completeGameMNY strat context
  where context  = contextContMNY iterator strat initialAction

-- Show output pathological end game
eqOutput iterator strat initialAction = generateIsEq $ repeatedCompleteGameEq iterator strat initialAction

-- Show output meeting in NY end game
eqOutputMYN iterator strat initialAction = generateIsEq $ repeatedCompleteGameMNYEq iterator strat initialAction


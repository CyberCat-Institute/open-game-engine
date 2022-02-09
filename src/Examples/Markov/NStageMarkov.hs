{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.Markov.NStageMarkov where

import           Data.Tuple.Extra (uncurry3)
import           OpenGames
import           OpenGames.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..), Location(..))

import           Control.Monad.State hiding (state,void, lift)
import qualified Control.Monad.State as ST

import Numeric.Probability.Distribution hiding (map, lift, filter)



-- Here we consider an even simpler version how to implement an N stage markov game where solely the payoffs depend on a stage; actions are identical

--------
-- Types

type EndState = Bool

type EndStateN = Int
----------
-- Payoffs

discountFactor = 0.9

-- | Payoff for a specific game state
payoffGameN 0 = 0.5
payoffGameN 1 = 0.3
payoffGameN 2 = 1
payoffGameN 3 = 0.2
payoffGameN _ = 0

----------------------
-- Auxiliary functions

-- The transition happens deterministically if one of the players does not play _Cooperate_
transitionEndStateDetermN :: EndStateN -> ActionPD -> ActionPD -> Stochastic EndStateN
transitionEndStateDetermN 3  _         _         = playDeterministically 3
transitionEndStateDetermN 0 Cooperate Cooperate  = playDeterministically 0
transitionEndStateDetermN 0 _         _          = playDeterministically 1
transitionEndStateDetermN 1 Cooperate Cooperate  = playDeterministically 1
transitionEndStateDetermN 1 _         _          = playDeterministically 2
transitionEndStateDetermN 2 Cooperate Cooperate  = playDeterministically 2
transitionEndStateDetermN 2 _         _          = playDeterministically 3



-------------
-- Open games

-- The baseline stage game: prisoner's dilemma
basicGame  :: OpenGame
                              StochasticStatefulOptic
                              StochasticStatefulContext
                              ('[Kleisli Stochastic (ActionPD, ActionPD, EndStateN) ActionPD,
                                 Kleisli Stochastic (ActionPD, ActionPD, EndStateN) ActionPD])
                              ('[[DiagnosticInfoBayesian (ActionPD, ActionPD, EndStateN) ActionPD],
                                 [DiagnosticInfoBayesian (ActionPD, ActionPD, EndStateN) ActionPD]])
                              (ActionPD, ActionPD, EndStateN)
                              ()
                              (ActionPD, ActionPD)
                              ()
basicGame = [opengame|

   inputs    : (dec1Old,dec2Old,endState) ;
   feedback  :      ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old,endState)    ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : payoffGameN endState ;

   inputs    :   (dec1Old,dec2Old,endState)   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : payoffGameN endState ;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   :      ;
  |]

-----------------
-- Complete Games

-- define the whole game, here with pathological endgame
completeGame = [opengame|

   inputs    : (dec1Old,dec2Old,gameStateOld) ;
   feedback  :  ;

   :----------------------------:
   inputs    : (dec1Old,dec2Old,gameStateOld);
   feedback  :      ;
   operation : basicGame ;
   outputs   : (dec1New,dec2New) ;
   returns   : ;

   inputs    :  (gameStateOld,dec1New,dec2New) ;
   feedback  :      ;
   operation : liftStochasticForward $ uncurry3 transitionEndStateDetermN;
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
-- NOTE the payoffs
strategyEq :: Kleisli Stochastic (ActionPD, ActionPD, EndStateN) ActionPD
strategyEq = Kleisli $
   (\case
       (_,_, 0) -> playDeterministically Defect
       -- ^ If in stage 0, play to get to stage 1
       (_,_, 1) -> playDeterministically Defect
       -- ^ If in stage 1, play to get to stage 2
       (_,_, 2) -> playDeterministically Cooperate
       -- ^ If in stage 2, stay there
       (_,_, _) -> uniform [Cooperate,Defect])
       -- ^ If in stage 3, play whatever you want

strategyAlt :: Kleisli Stochastic (ActionPD, ActionPD, EndStateN) ActionPD
strategyAlt = Kleisli $
       \(_,_, _) -> uniform [Cooperate,Defect]
       -- ^ Randomize

-- Strategy tuple for complete game
strategyTupleEq = strategyEq ::- strategyEq ::- Nil

-- Strategy tuple for complete game with randomization in first stage
strategyTupleAlt = strategyAlt ::- strategyAlt ::- Nil




-----------------------
-- Continuation payoffs

-- Extract continuation
extractContinuation :: StochasticStatefulOptic s () a () -> s -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- Extract next state (action)
extractNextState :: StochasticStatefulOptic s () (a,a,EndStateN) () -> s -> Stochastic (a,a,EndStateN)
extractNextState (StochasticStatefulOptic v _) x = do
  (z,a) <- v x
  pure a



-- Determine continuation for iterator, with the same repeated strategy, using the pathological endgame
determineContinuationPayoffs :: Integer
                             ->  List
                                    '[Kleisli Stochastic (ActionPD, ActionPD, EndStateN) ActionPD,
                                      Kleisli Stochastic (ActionPD, ActionPD, EndStateN) ActionPD]
                             -> (ActionPD,ActionPD,EndStateN)
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


--------------
-- Equilibrium

-- equilibria of repeated game with continuation payoff
repeatedCompleteGameEq iterator strat initialAction = evaluate completeGame strat context
  where context  = contextCont iterator strat initialAction


-- Show output
eqOutput iterator strat initialAction = generateIsEq $ repeatedCompleteGameEq iterator strat initialAction

-- NOTE: check the equilibrium for all possible states!

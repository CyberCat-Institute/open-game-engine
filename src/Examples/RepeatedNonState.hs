{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.RepeatedPDNonState where 


import           Engine.Engine hiding (StochasticStatefulOptic
                                      , StochasticStatefulBayesianOpenGame(..)
                                      , Agent(..)
                                      , dependentDecision
                                      , dependentEpsilonDecision
                                      , fromFunctions
                                      , fromLens
                                      , uniformDist
                                      , distFromList
                                      , pureAction
                                      , playDeterministically
                                      , discount
                                      )
import           Engine.BayesianGamesNonState
import           Preprocessor.Preprocessor


import Numeric.Probability.Distribution hiding (map, lift, filter)

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix Cooperate Cooperate   = 3
prisonersDilemmaMatrix Cooperate Defect  = 0
prisonersDilemmaMatrix Defect Cooperate  = 5
prisonersDilemmaMatrix Defect Defect = 1

discountFactor = 0.5


prisonersDilemmaCont = [opengame|

   inputs    : (dec1Old,dec2Old) ;
   feedback  : (prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 + return1,prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 + return2)     ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 + return1;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 + return2;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   : (return1,return2)     ;
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

-- For a an optic (derived from a play for a given strategy), and state (which is also the action), derive a new StateT
extractContinuation :: StochasticOptic s (Double,Double) a (Double,Double) -> s -> (Double,Double) -> Stochastic (Double,Double)
extractContinuation (StochasticOptic v u) x r = do
  (z,_) <-  (v x)
  u z r

-- For  an optic (derived from a play for a given strategy), and state derive the action which was played, which in turn is then the next state
extractNextState :: StochasticOptic s t a b -> s -> Stochastic a
extractNextState (StochasticOptic v _) x = do
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
     -> (Double,Double)
     -> Stochastic (Double,Double)
continuationPayoffs iterator strat action return@(r1,r2)
  | iterator == 1 = pure return -- extractContinuation (execute strat) action return
  | otherwise     = do
      (r1',r2') <- extractContinuation (execute strat) action (r1, r2)
      actionNew <-  nextState strat action
      continuationPayoffs (pred iterator) strat actionNew (r1'*discountFactor,r2'*discountFactor)
  where execute strat'           = play prisonersDilemmaCont strat' -- results in an optic
        nextState strat' action' = extractNextState (execute strat') action'


-- Gives the context with the initial condition, the state, and for an action taken determines the continuation payoff
contextCont
  :: (Eq t1, Num t1, Enum t1) =>
     t1
     -> List
          '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
            Kleisli Stochastic (ActionPD, ActionPD) ActionPD]
     -> (ActionPD, ActionPD)
     -> StochasticContext
          (ActionPD, ActionPD) (Double,Double) (ActionPD, ActionPD) (Double,Double)
contextCont iterator strat initialAction = StochasticContext (pure ((),initialAction)) (\_ action -> continuationPayoffs iterator strat action (0,0))

-- evaluate the one stage game with a given strategy and a given initial state, as context, use the payoff derived from continuously playing that stage game
evaluateIteratedPD iterator strat initialAction = generateOutput $ evaluate prisonersDilemmaCont strat context
  where context = contextCont iterator strat initialAction

checkEq iterator initialAction = generateIsEq $  evaluate prisonersDilemmaCont strategyTuple context
  where context = contextCont iterator strategyTuple initialAction



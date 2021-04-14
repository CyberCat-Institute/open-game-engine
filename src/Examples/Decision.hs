{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.Decision
  ( checkSingleDecisionVerbose
  ) where

import GHC.Generics

import Engine.Engine
import Preprocessor.Preprocessor

---------------------------
-- 0. Game Representations
--    Case of no prior info
---------------------------
-- | A single decision with no prior input or output
-- Requires a list of actions, and a payoff function
singleDecisionVerbose actionSpace payoffFunction = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const actionSpace);
   outputs   : decisionPlayer1 ;
   returns   : payoffFunction decisionPlayer1     ;
   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

-- | The same decision in the reduced style, i.e. ignoring empty fields
-- Requires a list of actions, and a payoff function
singleDecisionReduced actionSpace payoffFunction = [opengame|
   operation : dependentDecision "player1" (const actionSpace);
   outputs   : decisionPlayer1 ;
   returns   : payoffFunction decisionPlayer1     ;
  |]

 -- | A single decision using the parameterized version, _decisionNoObs_,  as presented in _AtomicGames_
 -- NOTE: this version requires input from the outside, a return type
singleDecisionAtomic actionSpace payoffFunction playerName = decisionNoObs actionSpace payoffFunction playerName

-------------------------------
-- 1. Game Representations
--    Case of prior information
-------------------------------

-- | A single decision with prior input
-- Requires a list of actions, and a payoff function
-- NOTE: The payoff here depends on the external information
singleDecisionPriorVerbose actionSpace payoffFunction = [opengame|
   inputs    : x    ;
   feedback  :      ;

   :----------------------------:
   inputs    : x    ;
   feedback  :      ;
   operation : dependentDecision "player1" (\x -> actionSpace);
   outputs   : decisionPlayer1 ;
   returns   : payoffFunction decisionPlayer1  x  ;
   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]

-- | The same decision in the reduced style, i.e. ignoring empty fields
-- Requires a list of actions, and a payoff function
singleDecisionPriorReduced actionSpace payoffFunction = [opengame|
   inputs    : x    ;
   :----------------------------:
   inputs    : x    ;
   operation : dependentDecision "player1" (\x -> actionSpace);
   outputs   : decisionPlayer1 ;
   returns   : payoffFunction decisionPlayer1     ;
  |]

 -- | A single decision using the parameterized version, _decision_, as presented in _AtomicGames_
 -- NOTE: this version requires input from the outside, a return type
singleDecisionPriorAtomic actionSpace payoffFunction playerName = decision actionSpace payoffFunction playerName

-------------------------------
-- 2. Game Representations
--    Stochastic environment
-------------------------------
-- The idea is that the decision may depend on some outside parameter

-- | We define a uniform distribution for a peak; check the payoff function below for information
-- We can use the auxiliary _uniformDist_ which creates a uniform distribution from a list.
distributionUniformPeak = uniformDist actionSpace

-- | As an alternative, we create a custom-made distribution which expects a list of tuples (outcome, weight)
-- We can use the auxiliary _distFromList_
distributionCustomPeak = distFromList [(1,2),(2,2),(3,4),(5,5),(6,4),(7,2),(8,2),(9,1)]

-- | This game represents a simple stochastic process and expects a distribution as input
stochasticEnv distribution = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : nature distribution;
   outputs   : draw ;
   returns   :      ;
   :----------------------------:

   outputs   :  draw ;
   returns   :      ;
  |]

-- | NOTE: the same functionality as above is available as an auxiliary function -- natureDraw_
stochasticEnvAtomic distribution = natureDraw distribution

-- | Next, we combine this with a decision stage. Such combinations will be useful later in other contexts as well. 
singleDecStoch distribution actionSpace payoffFunction = [opengame|
   inputs    :     ;
   feedback  :     ;

   :----------------------------:
   inputs    :     ;
   feedback  :     ;
   operation : stochasticEnv distribution ;
   outputs   : draw ;
   returns   :      ;

   inputs    : draw ;
   feedback  :      ;
   operation : singleDecisionPriorVerbose actionSpace payoffFunction;
   outputs   :      ;
   returns   :      ;
   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-------------------------------
-- 2. Example parameters
-------------------------------
-- | An action space from 1 - 10
actionSpace = [1.0..10.0]

distribution = uniformDist actionSpace

-- | Some arbitrary payoff function with a clear max
payoffFunction peak dec = - (peak - dec)**2

-- | Instantiate the single decision with parameters
-- We use the _payoff_ function with a peak at 5
gameSingleDecisionVerbose = singleDecisionVerbose actionSpace (payoffFunction 5)

-- | Instantiate the single decision with random parameter
gameSingleDecisionStoch  = singleDecStoch distribution actionSpace payoffFunction

-------------------------------
-- 3. Analysis
-------------------------------
-- | Check whether a given action is in equilibrium
-- NOTE: _void_ represents a context relative to which a game is evaluated. As the game used has
-- no incoming or outcoming information, the context is empty (i.e. "void")
-- NOTE:This game requires you to provide a strategy. Here this is just a simple action. But this can be a more complicated object
checkSingleDecisionVerbose strat = generateIsEq $ evaluate gameSingleDecisionVerbose strat void

checkSingleDecisionStochastic strat = generateIsEq $ evaluate gameSingleDecisionStoch strat void

-- Strategies

-- | We use the helper function _pureAction_ to determine a pure strategy
-- here just the peak
pure5 :: List '[Kleisli Stochastic () Double]
pure5 = pureAction 5.0 ::- Nil

-- | Next, we define a strategy which makes an observation and uses that observation as the action
-- again using the helper _pureAction_
peak :: List '[Kleisli Stochastic Double Double]
peak  = (Kleisli $ (\x -> playDeterministically x) )  ::- Nil

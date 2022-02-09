{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.Decision
  ( isOptimalSingleDecisionVerbose
  , pureIdentity
  , isOptimalSingleDecisionStoch
  , peak
  ) where

import GHC.Generics

import OpenGames
import OpenGames.Preprocessor

---------------------------------------------
-- 0. A single decision w/o prior information
-- We depict a single decision without
-- interaction to the outside
---------------------------------------------
-- 0.0. Game representations

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

------------------
-- 0.1. Parameters
-- Next, we provide additional modelling parameters

-- | An action space from 1 - 10
actionSpace = [1.0..10.0]

-- | Some arbitrary payoff function with a clear max
payoffFunction peak dec = - (peak - dec)**2

-- | Instantiate the single decision with parameters
-- We use the _payoff_ function with a peak at 5
gameSingleDecisionVerbose = singleDecisionVerbose actionSpace (payoffFunction 5)

----------------
-- 0.2. Analysis
-- We analyze the game and test whether certain actions are optimal
-- Here, this is trivial, but it is useful to see it first at that simple level


-- | This function defines a checker on the game _gameSingleDecisionVerbose_
-- It expects a strategy, here just a single action, as input and outputs information whether the action is optimal
-- This is done by the _evaluate_ function. In a game with several players the same _evaluate_ will check for an equilibrium condition
-- The _generateisEq_ transforms this output into an easier to digest format.
-- NOTE: _void_ represents a context relative to which a game is evaluated. As the game used has
-- no incoming or outcoming information, the context is empty (i.e. "void")
isOptimalSingleDecisionVerbose strat = generateIsEq $ evaluate gameSingleDecisionVerbose strat void


-- | Next, we define a strategu. We use the helper function _pureAction_ to determine a pure strategy
-- This is as simples as it gets, the strategy expects an action and will play this
-- action with certainty
pureIdentity :: Double -> List '[Kleisli Stochastic () Double]
pureIdentity action = pureAction action ::- Nil

-- | Now, we are ready to actually run this game and the optimality check. Example usages:
-- isOptimalSingleDecisionVerbose (pureIdentity 4), or
-- isOptimalSingleDecisionVerbose (pureIdentity 5),

----------------------------------
-- 1. A single decision with prior
-- information and a stochastic
-- environment
----------------------------------
-- 1.1 Game representation: Extended decision

-- | A single decision with prior input
-- This extends the previous decision under 0
-- Requires a list of actions, and a payoff function
-- NOTE: The payoff here depends on the external information _x_
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

-----------------------------------
-- 1.2. Game representation: Nature

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

-----------------------------------------------
--  1.3. Game representation: The combined game

-- | We combine the stochastic process with a decision stage.
-- Such combinations are important as other games will rely on them heavily
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

-----------------
-- 1.4 Parameters

-- | We define a uniform distribution for a peak; check the payoff function above for information
-- We can use the auxiliary _uniformDist_ which creates a uniform distribution from a list.
distributionUniformPeak = uniformDist actionSpace

-- | As an alternative, we create a custom-made distribution which expects a list of tuples (outcome, weight)
-- We can use the auxiliary _distFromList_
distributionCustomPeak = distFromList [(1,2),(2,2),(3,4),(5,5),(6,4),(7,2),(8,2),(9,1)]

-- | Instantiate the single decision with random parameter
gameSingleDecisionStoch  = singleDecStoch distributionUniformPeak actionSpace payoffFunction


isOptimalSingleDecisionStoch strat = generateIsEq $ evaluate gameSingleDecisionStoch strat void

----------------
-- 1.5. Analysis
-- | Next, we define a strategy which makes an observation and uses that observation as the action
-- again using the helper _pureAction_
peak :: List '[Kleisli Stochastic Double Double]
peak  = (Kleisli $ (\x -> playDeterministically x) )  ::- Nil


-- | Example usage
-- isOptimalSingleDecisionStoch peak



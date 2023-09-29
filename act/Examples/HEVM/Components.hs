{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.HEVM.Components
  where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Engine.BayesianGames (addPayoffsReturns)

{-
Basic Components needed to model different txs
-}


-----------------------------
-- 1. Single player decisions

-- Single player decision
-- The structure should cover different kinds of transactions
-- Only the account difference affects payoffs here
-- NOTE I assume that the modeller specifies the kind of transaction; in so far as there are multiple parameters to choose, we will model this step by step.
singlePlayerTransactionChoice name actionSpace replaceMeWithAccountDiff  = [opengame|

    inputs    :  state, privateValue ;
    feedback  :   ;

    :---------------------------:

    // Decision to choose parameters for a specific transaction; in the case of a deposit, how much to deposit
    // This generates an account balance difference to be directly taken into account here
    // Account difference operation tbd
    inputs    :  state, privateValue ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  dec ;
    returns   :  replaceMeWithAccountDiff stateNew ;

    :---------------------------:

    outputs   :  dec ;
    returns   :  stateNew  ;
  |]

------------------------------------
-- 2. Accounting for further payoffs
-- NOTE The player might derive further payoffs from at least three different sources
-- 1. Actions by other players affect his utility as well
-- 2. He receives additional utility directly from the action; possibly with only privately known values
-- 3. He receives direct additional utility from the state change (possible further consequences outside the model).

-- Account for additional value directly derived from action
-- NOTE value given exogenously
addPrivateValueDirectExogenous name payoffFunctionDirect privateValueDirect = [opengame|

    inputs    : dec ;
    feedback  :     ;

    :---------------------------:

    // Computes possible side values not explicitly accounted for in the account difference
    inputs    :  dec ;
    feedback  :   ;
    operation :  forwardFunction (payoffFunctionDirect privateValueDirect) ;
    outputs   :  privatePayoffDirect ;
    returns   :   ;

    // Book-keeping: Assign the private "direct" payoff to the player. 
    inputs    :  privatePayoffDirect ;
    feedback  :   ;
    operation :  addPayoffs name;
    outputs   :   ;
    returns   :   ;

   :---------------------------:

    outputs   :   ;
    returns   :   ;
  |]

-- Account for additional value directly derived from action
-- NOTE value given endogenously
addPrivateValueDirectEndogenous name payoffFunctionDirect = [opengame|

    inputs    : dec, privateValueDirect ;
    feedback  :     ;

    :---------------------------:

    // Computes possible side values not explicitly accounted for in the account difference
    inputs    :  dec, privateValueDirect ;
    feedback  :   ;
    operation :  forwardFunction payoffFunctionDirect ;
    outputs   :  privatePayoffDirect ;
    returns   :   ;

    // Book-keeping: Assign the private "direct" payoff to the player. 
    inputs    :  privatePayoffDirect ;
    feedback  :   ;
    operation :  addPayoffs name;
    outputs   :   ;
    returns   :   ;

   :---------------------------:

    outputs   :   ;
    returns   :   ;
  |]


{--
-- Account for additional value indirectly derived from state
-- NOTE value given exogenously
addPrivateValueStateExogenously name payoffFunctionState privateValueState = [opengame|

    inputs    :  ;
    feedback  :     ;

    :---------------------------:

    // Book-keeping: Assign the private "state" payoff to the player. 
    inputs    :   ;
    feedback  :   ;
    operation :  addPayoffsReturns name;
    outputs   :   ;
    returns   :  privatePayoffState ;

    // Private Payoff derived from state change
    inputs    :   ;
    feedback  :  privatePayoffState ;
    operation :  backwardFunction (payoffFunctionState privateValueState) ;
    outputs   :   ;
    returns   :  stateNew ;

   :---------------------------:

    outputs   :   ;
    returns   :  stateNew ;
  |]

-- Account for additional value indirectly derived from state
-- NOTE value given exogenously
addPrivateValueStateEndogenously name payoffFunctionState = [opengame|

    inputs    : privateValueState ;
    feedback  :     ;

    :---------------------------:

    // Book-keeping: Assign the private "state" payoff to the player. 
    inputs    :   ;
    feedback  :   ;
    operation :  addPayoffsReturns name;
    outputs   :   ;
    returns   :  privatePayoffState ;

    // Private Payoff derived from state change and privateValueState
    inputs    :  privateValueState ;
    feedback  :  privatePayoffState ;
    operation :  fromLens id payoffFunctionState ;
    outputs   :  dummyOutput ;
    returns   :  stateNew ;

   :---------------------------:

    outputs   :   ;
    returns   :  stateNew ;
  |]



-------------------------------------------------------
-- FIXME ONLY FOR TESTING PURPOSE; ERASE ONCE CLARIFIED
singleDecisionVerbose name addOne  = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "test" (const actionSpace) ;
   outputs   : dec ;
   returns   : dec ;

   inputs    :      ;
   feedback  :      ;
   operation : addPayoffsReturns name ;
   outputs   :  ;
   returns   : payoff ;

   inputs    :      ;
   feedback  : payoff     ;
   operation : backwardFunction addOne ;
   outputs   :  ;
   returns   : testInput ;
   :----------------------------:

   outputs   :       ;
   returns   : testInput    ;
  |]

test = 5

testGame f = evaluate (singleDecisionVerbose "test" f)

actionSpace = [1]

strategyTest
  :: List
       '[Kleisli Stochastic a Double]
strategyTest = (pureAction 1.5) :- Nil

outputTest con f = generateOutput $ (testGame f) strategyTest con

--------------------------------------------------------
-}  

-------------------------
-- 3. Advancing the state

advancingState contractFunctionality = [opengame|

    inputs    :  state, parameters  ;
    feedback  :   ;

    :---------------------------:

    inputs    :  state, parameters ;
    feedback  :  ;
    operation :  forwardFunction contractFunctionality ;
    outputs   :  newState ;
    returns   : ;

    :---------------------------:

    outputs   :  newState ;
    returns   :  ;
  |]


----------------------
-- 4. Composed modules

-- Player observes private information, chooses a transaction, account balances are update and receives (possibly) additional utility from his action
playerWithAdditionalValue name probDistribution actionSpace replaceMeWithAccountDiff contractFunctionality payoffFunctionDirect = [opengame|

    inputs    :  state, parameters ;
    feedback  :  ;

    :---------------------------:

    inputs    :   ;
    feedback  :   ;
    operation :  natureDraw probDistribution ;
    outputs   :  privateValueDirect ;
    returns   :   ;

    inputs    :  state, privateValueDirect  ;
    feedback  :   ;
    operation :  singlePlayerTransactionChoice name actionSpace replaceMeWithAccountDiff ;
    outputs   :  dec ;
    returns   :  stateNew ;

    inputs    :  state, parameters ;
    feedback  :   ;
    operation :  advancingState contractFunctionality ;
    outputs   :  stateNew ;
    returns   :   ;

    inputs    :  dec, privateValueDirect ;
    feedback  :   ;
    operation :  addPrivateValueDirectEndogenous name payoffFunctionDirect ;
    outputs   :   ;
    returns   :   ;



   :---------------------------:

    outputs   :  stateNew ;
    returns   :   ;
  |]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.HEVM.Components
  where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import OpenGames.Engine.BayesianGames (addPayoffsReturns)

{-
Basic Components needed to model the game
-}


-- Single player decision
-- The structure should cover different kinds of transactions
-- Only the account difference affects payoffs
singlePlayerTransactionChoice name actionSpace replaceMeWithAccountDiff  = [opengame|

    inputs    :  state  ;
    feedback  :   ;

    :---------------------------:

    // Decision to choose parameters for a specific transaction; in the case of a deposit, how much to deposit
    // This generates an account balance difference to be directly taken into account here
    // Account difference operation tbd
    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  deposits ;
    returns   :  replaceMeWithAccountDiff stateNew ;

    :---------------------------:

    outputs   :  deposits ;
    returns   :  stateNew  ;
  |]

{-
-- Single player decision with private values
-- The structure should cover different kinds of transactions
-- NOTE I assume that the modeller specifies the kind of transaction; in so far as there are multiple parameters to choose, we will model this step by step. 
-- NOTE I am making this extra verbose just to single out the different parts; see comments below
-- NOTE I am assuming extra value from a transaction comes directly from the choice; we could also model extra payoffs conditional on the state 
singlePlayerTransactionChoicePrivateValue name actionSpace replaceMeWithAccountDiff payoffFunctionDirect privateValueDirect payoffFunctionState privateValueState= [opengame|

    inputs    :  state  ;
    feedback  :   ;

    :---------------------------:

    // Decision to choose parameters for a specific transaction; in the case of a deposit, how much to deposit
    // This generates an account balance difference to be directly taken into account here
    // Account difference operation tbd
    inputs    :  state ;
    feedback  :   ;
    operation :  dependentDecision name actionSpace ;
    outputs   :  deposits ;
    returns   :  replaceMeWithAccountDiff stateNew ;

    // Computes possible side values not explicitly accounted for in the account difference
    inputs    :  deposits ;
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

    outputs   :  deposits ;
    returns   :  stateNew ;
  |]


testGame name actionSpace replaceMeWithAccountDiff payoffFunctionDirect privateValueDirect payoffFunctionState privateValueState= [opengame|

    inputs    :   ;
    feedback  :   ;

    :---------------------------:

    // Book-keeping: Assign the private "state" payoff to the player. 
    inputs    :   ;
    feedback  :   ;
    operation :  addPayoffsReturns name;
    outputs   :   ;
    returns   :  privatePayoffState ;

    inputs    :   ;
    feedback  :  privatePayoffState ;
    operation :  backwardFunction id ;
    outputs   :   ;
    returns   :  stateNew ;

   :---------------------------:

    outputs   :   ;
    returns   :  stateNew ;
  |]


// Private Payoff derived from state change
    inputs    :   ;
    feedback  :  privatePayoffState ;
    operation :  backwardFunction (payoffFunctionState privateValueState) ;
    outputs   :   ;
    returns   :  stateNew ;

-}

-- FIXME testing this decision 
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

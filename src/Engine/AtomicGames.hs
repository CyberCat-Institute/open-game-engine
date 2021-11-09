{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Engine.AtomicGames
 ( decision
 , decisionNoObs
 , forwardFunction
 , backwardFunction
 , natureDraw
 , liftStochasticForward
 ) where

import Language.Haskell.TH

import Engine.BayesianGames
import Engine.OpenGames
import Engine.OpticClass
import Preprocessor.THSyntax
import Preprocessor.AbstractSyntax
import Preprocessor.Compile

---------------------------------------
-- 0. A single action making a generic -- parameterized -- decision
decision actionSpace payoffFunction playerName = [opengame|

    inputs    : x ;
    feedback  :   ;

    :-----:
    inputs    : x ;
    feedback  :   ;
    operation : dependentDecision playerName (\y -> actionSpace) ;
    outputs   : y ;
    returns   : payoffFunction y x r ;
    :-----:

    outputs  : y ;
    returns  : r ;

|]

-- 1. A single action making a decision without prior observations
decisionNoObs actionSpace payoffFunction playerName = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : dependentDecision playerName (\y -> actionSpace) ;
    outputs   : y ;
    returns   : payoffFunction y r ;
    :-----:

    outputs  : y ;
    returns  : r ;

|]


-- 2. "Forward" (covariant) function: from past to future
forwardFunction function = [opengame|

    inputs    : x ;
    feedback  :   ;

    :-----:
    inputs    : x  ;
    feedback  :   ;
    operation : fromFunctions function id ;
    outputs   : y ;
    returns   :   ;
    :-----:

    outputs  : y ;
    returns  :   ;

|]

 -- 3. "Backward" (contravariant) function: from future to past
backwardFunction function = [opengame|

    inputs    :   ;
    feedback  : s ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : fromFunctions id function ;
    outputs   : s ;
    returns   : r ;
    :-----:

    outputs  :    ;
    returns  :  r ;

|]

-- 4. Drawing from a probability distribution
natureDraw distribution =  [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature distribution ;
    outputs   : draw ;
    returns   :  ;
    :-----:

    outputs   :  draw  ;
    returns   :    ;

|]

-- 5. lift a stochasticProcess forward
liftStochasticForward process =  [opengame|

    inputs    : x ;
    feedback  :   ;

    :-----:
    inputs    : x ;
    feedback  :   ;
    operation : liftStochastic process;
    outputs   : draw ;
    returns   :   ;
    :-----:

    outputs   : draw  ;
    returns   :    ;

|]


generateGame "pureDecision2" ["actionSpace","payoffFunction","playerName"] $
  (Block ["observation"] []
         [mkLine [[|observation|]] [] [|dependentDecision playerName (\y -> actionSpace)|] ["action"] [[|payoffFunction observation action returns|]]]
         [[|action|]] ["returns"])


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Examples.Decision where

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
    returns   : payoffFunction y ;
    :-----:

    outputs  : x ;
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
    returns   : payoffFunction y ;
    :-----:

    outputs  : x ;
    returns  : r ;

|]


  

generateGame "pureDecision2" ["actionSpace","payoffFunction","playerName"] $
  (Block ["observation"] []
         [Line [[|observation|]] [] [|dependentDecision playerName (\y -> actionSpace)|] ["action"] [[|payoffFunction observation action returns|]]]
         [[|action|]] ["returns"])


{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric #-}
module Examples.Decision where

import GHC.Generics
import Engine.Engine

import Preprocessor.THSyntax
import Preprocessor.Compile
import Preprocessor.AbstractSyntax


-- A single decision with no prior input or output
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

-- The same decision in the reduced style, i.e. ignoring empty fields
-- Requires a list of actions, and a payoff function
singleDecisionReduced actionSpace payoffFunction = [opengame|
   operation : dependentDecision "player1" (const actionSpace);
   outputs   : decisionPlayer1 ;
   returns   : payoffFunction decisionPlayer1     ;
  |]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.ExploreStatePayoffs where 


import           Engine.Engine
import           Preprocessor.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..),prisonersDilemmaMatrix)

import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST





prisonersDilemmaBasic = [opengame|

   inputs    :  ;
   feedback  :  ;

   :----------------------------:
   inputs    :   ;
   feedback  :   ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :    ;
   feedback  :    ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]


prisonersDilemmaComposed = [opengame|

   inputs    :  ;
   feedback  :  ;

   :----------------------------:
   inputs    :   ;
   feedback  :   ;
   operation : prisonersDilemmaBasic ;
   outputs   : ;
   returns   : ;

   inputs    :    ;
   feedback  :    ;
   operation : prisonersDilemmaBasic ;
   outputs   : ;
   returns   : ;


   :----------------------------:

   outputs   :  ;
   returns   :  ;

   |]


prisonersDilemmaComposed2 = prisonersDilemmaBasic >>> prisonersDilemmaBasic

composedEq strat =generateIsEq $  evaluate prisonersDilemmaComposed strat void

stratSimple =  Kleisli $ const $ playDeterministically Cooperate

stratTupleSimple = stratSimple ::- stratSimple ::- stratSimple ::- stratSimple ::- Nil

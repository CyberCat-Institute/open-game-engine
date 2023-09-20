{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.HEVM where

import Data.Text
import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.Engine
import EVM.Types

balance :: VM -> String -> Double
balance vm _ = undefined

actDecision name strategies =
  [opengame|
  inputs : observedInput ;
  :---:

  inputs : observedInput ;
  operation : dependentDecision name (const strategies) ;
  outputs   : tx ;
  returns  : balance finalState name ;

  :---:
  outputs : tx ;
  returns : finalState;
|]

append = (++)

bundles = undefined

runBlockchain = [opengame|
  inputs : ;
  :---:

  operation : actDecision "Marx" (bundles 10) ;
  outputs : allTx ;
  returns : finalState ;

  inputs : allTx ;
  operation : fromFunctions (initSendAndRun) id ;
  outputs : finalState ;
|]

-- foo = evaluate runBlockchain
--     (Kleisli (const (pure [swap0 0, swap1 0])) :- Nil) void

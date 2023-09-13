{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenGames.Engine.Stateful where

import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.Engine
-- import Control.Monad.State

data Transaction = Tx Int

data State = State { contracts :: [Int]}

sendAndRun :: Transaction -> State -> State
sendAndRun = undefined

load :: [Int] -> State -> State
load = undefined

strategy :: [Transaction]
strategy = undefined

balance :: State -> String -> Double
balance st name = undefined --balance (lookup "marx" st)

-- actDecision1 :: String -> [Tx] -> OG .....
actDecision name strategies = [opengame|
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

runBlockchain = [opengame|
  inputs : initialState ;
  :---:

  operation : actDecision "Alice" [Tx 1, Tx 2] ;
  outputs : aliceTx ;
  returns : finalState ;

  operation : actDecision "Bob" [Tx 3, Tx 4] ;
  outputs : bobTx ;
  returns : finalState ;

  inputs : (append aliceTx bobTx), initialState ;
  operation : uncurry sendAndRun ;
  outputs : finalState ;
|]

--
-- playerAutomatic usd =
--   [opengame|
--   inputs   : globalState, ;
--   feedback : ;
--   :-------:
--
--   operation : dependentDecision "AllPlayers" (const strategy) ;
--   outputs   : transactions ;
--   returns   : (payoff finalState)
--
--   inputs    : globalState, transactions;
--   feedback  : ;
--   operation : forwardFunction (uncurry sendAndRun) ;
--   outputs   : finalState ;
--   returns   : ;
--
--   :-------:
--   outputs: ;
--   returns : ;
-- |]
--

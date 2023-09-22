{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.HEVM where

import Act.Prelude
import Control.Monad.Trans.State.Strict
import Data.DoubleWord
import Debug.Trace
import EVM.Exec
import EVM.Fetch (zero)
import EVM.Stepper (evm, interpret, runFully)
import EVM.TH
import EVM.Types
import OpenGames
import OpenGames.Preprocessor
import Optics.Core (view, (%))

sendAndRun :: EthTransaction -> VM -> VM
sendAndRun tx = execState $ do
  makeTxCall tx
  run

deposit amt =
  EthTransaction
    0xabcd
    0x1234
    "deposit()"
    []
    amt
    100000000

dummyTx :: Int256 -> EthTransaction
dummyTx amt =
  EthTransaction
    0xabcd
    0x1234
    "retrieve(uint256)"
    [AbiInt 256 amt]
    0
    100000000

transactionList :: Int256 -> [EthTransaction]
transactionList max = [dummyTx n | n <- [1 .. max]]

balance :: VM -> String -> Double
balance st name = trace ("memory size: " ++ show (view (#state % #memorySize) st) ++ "memory: " ++ show (view (#state % #memory) st)) 0

-- actDecision1 :: String -> [Tx] -> OG .....
actDecision name strategies =
  [opengame| inputs : observedInput ;
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

-- runBlockchain = [opengame|
--   inputs : initialState ;
--   :---:
--
--   operation : actDecision "Alice" [Tx 1, Tx 2] ;
--   outputs : aliceTx ;
--   returns : finalState ;
--
--   operation : actDecision "Bob" [Tx 3, Tx 4] ;
--   outputs : bobTx ;
--   returns : finalState ;
--
--   inputs : (append (pure aliceTx) (pure bobTx)), initialState ;
--   operation : fromFunctions (uncurry sendAndRun) id ;
--   outputs : finalState ;

-- | ]
playerAutomatic globalState =
  [opengame|
  inputs   : ;
  feedback : ;
  :-------:

  inputs    : globalState ;
  operation : fromFunctions (sendAndRun (deposit 100)) id ;
  outputs   : withFunds ;

  operation : dependentDecision "AllPlayers" (const (transactionList 2)) ;
  outputs   : transactions ;
  returns   : (balance finalState "a") ;

  inputs    : transactions, withFunds;
  feedback  : ;
  operation : forwardFunction (uncurry sendAndRun) ;
  outputs   : finalState ;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

initial :: VM
initial = loadContracts [("Piggybank", "solitidy/Withdraw.sol")]

outcome = evaluate (playerAutomatic initial) (Kleisli (const $ pure (dummyTx 10)) :- Nil) void

testExec = do
  makeTxCall (deposit 100)
  run

-- makeTxCall (dummyTx 20)
--  run

interp = interpret (zero 0 (Just 0)) initial (evm testExec >> runFully)

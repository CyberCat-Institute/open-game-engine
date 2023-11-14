{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.HEVM where

import EVM.Prelude
import Control.Monad.Trans.State.Strict
import Data.DoubleWord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map
import Debug.Trace
import EVM.Exec
import EVM.Format
import EVM.Fetch (zero)
import EVM.Stepper (evm, interpret, runFully)
import EVM.TH
import EVM.Types
import OpenGames hiding (fromFunctions, dependentDecision, fromLens)
import OpenGames.Engine.HEVMGames
import OpenGames.Engine.Diagnostics
import OpenGames.Preprocessor
import Optics.Core (view, (%))
--
--  primary goals:
--  - import lido
--  - run transactions
--    - staking
--    - unstaking
--  - import curve contract
--    - convert stEth to Eth
--  - Extracting payoffs (???)
--  - final goal: combine curve + lido in OG
--
--  Follow up goals:
--  - Ergonomics around modeling contracts

import Control.Monad.ST

-- send and run a transaction on the EVM state
sendAndRun' :: EthTransaction -> EVM RealWorld (VM RealWorld)
sendAndRun' tx = do
  EVM.TH.makeTxCall tx
  run

-- exectute the EVM state in IO
sendAndRun :: EthTransaction
            -> VM RealWorld -> HEVMState (VM RealWorld)
sendAndRun tx st = do put st
                      sendAndRun' tx
                      get

deposit amt =
  EthTransaction
    (LitAddr 0xabcd)
    (LitAddr 0x1234)
    "deposit()"
    []
    amt
    100000000

dummyTx :: Int256 -> EthTransaction
dummyTx amt =
  EthTransaction
    (LitAddr 0xabcd)
    (LitAddr 0x1234)
    "retrieve(uint256)"
    [AbiInt 256 amt]
    0
    100000000

transactionList :: Int256 -> [EthTransaction]
transactionList max = [dummyTx n | n <- [1 .. max]]

balance :: VM s -> String -> Double
balance st name = trace ("memory size: " ++ show (view (#state % #memorySize) st) ++ "memory: " ++ show (view (#state % #memory) st)) 0

-- actDecision1 :: String -> [Tx] -> OG .....
actDecision name strategies =
  [opengame| inputs : observedInput ;
  :---:

  inputs : observedInput ;
  operation : hevmDecision name (strategies) ;
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
playerManual globalState =
  [opengame|
  inputs   : ;
  feedback : ;
  :-------:

  inputs    : globalState ;
  operation : fromLensM (sendAndRun (deposit 100)) (const pure) ;
  outputs   : withFunds ;

  operation : hevmDecision "AllPlayers" (transactionList 2) ;
  outputs   : transactions ;
  returns   : balance finalState "a";

  inputs    : transactions, withFunds;
  feedback  : ;
  operation : fromLensM (uncurry sendAndRun) (const pure) ;
  outputs   : finalState ;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

-- playerAutomatic =
--   [opengame|
--   inputs   : ;
--   feedback : ;
--   :-------:
--
--   inputs    : ;
--   operation : fromLensM (const (sendAndRun' (deposit 100))) (const pure) ;
--   outputs   : ;
--
--   operation : hevmDecision "AllPlayers" (transactionList 2) ;
--   outputs   : transactions ;
--   returns   : balance finalState "a";
--
--   inputs    : transactions;
--   feedback  : ;
--   operation : fromLensM (sendAndRun') (const pure) ;
--   outputs   : finalState ;
--   returns   : ;
--
--   :-------:
--   outputs: ;
--   returns : ;
-- |]

initial :: ST s (VM s)
initial = loadContracts [("Piggybank", "solidity/Withdraw.sol")]

instance (Apply OpenGames.Engine.Diagnostics.PrintOutput
                (DiagnosticInfoBayesian () EthTransaction)
                [Char]) where
   apply a b = showDiagnosticInfoL [b]


outcome = do
  i <- stToIO initial
  let term :- Nil =  evaluate (playerManual i) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term i
  tevaluated <- stToIO t'
  generateOutput (tevaluated :- Nil)
  -- let tio = stToIO t'
  -- let g = generateOutput term

testExec = do
  evm $ makeTxCall (deposit 100)
  runFully
  evm $ makeTxCall (dummyTx 20)
  runFully

showVM :: VM s -> Text
showVM vm = T.unlines
  [ "Contracts:"
  , indent 2 . T.unlines . Map.elems $ Map.mapWithKey (\a c -> T.pack (show a) <> " :\n  " <> showContract c) vm.env.contracts
  -- , "Storage: " <> (formatExpr vm.env.storage)
  , "CallValue: " <> (formatExpr vm.state.callvalue)
  , "Result: " <> (T.pack $ show vm.result)
  ]

showContract :: Contract -> Text
showContract c = T.unlines
  [ "balance: " <> (T.pack $ show c.balance)
  ]

-- interp = do
--   vm <- interpret (zero 0 (Just 0)) initial (testExec)
--   T.putStrLn (showVM vm)

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import EVM (emptyContract, exec1)
import OpenGames hiding (fromFunctions, dependentDecision, fromLens)
import OpenGames.Engine.HEVMGames
import OpenGames.Engine.Diagnostics
import OpenGames.Preprocessor hiding (Lit)
import Optics.Core (view, (%?), (.~), (%), (&), over, at, set, preview)

import GHC.Float
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

run' :: EVM s (VM s)
run' = do
  vm <- get
  case vm.result of
    Nothing -> exec1 >> run'
    Just (HandleEffect (Query (PleaseAskSMT (Lit c) _ cont))) -> cont (Case (c > 0)) >> run'
    Just _ -> pure vm

-- send and run a transaction on the EVM state
sendAndRun' :: EthTransaction -> EVM RealWorld (VM RealWorld)
sendAndRun' tx = do
  EVM.TH.makeTxCall tx
  vm <- run'
  traceM (show vm.result)
  pure vm


-- exectute the EVM state in IO
sendAndRun :: EthTransaction
            -> VM RealWorld -> HEVMState (VM RealWorld)
sendAndRun tx st = do put st
                      sendAndRun' tx
                      get

userContractAddress = LitAddr 0x1234
withdrawContractAddress = LitAddr 0xabcd

deposit :: EthTransaction
deposit =
  EthTransaction
    withdrawContractAddress
    userContractAddress
    "deposit()"
    []
    1_000
    10_000_000

dummyTx :: Int256 -> EthTransaction
dummyTx amt =
  EthTransaction
    withdrawContractAddress
    userContractAddress
    "retrieve(uint256)"
    [AbiInt 256 amt]
    0
    100000000

transactionList :: Int256 -> [EthTransaction]
transactionList max = [dummyTx n | n <- [1 .. max]]

balance :: VM s -> String -> Double
balance st name =
  let contract = Map.lookup userContractAddress st.env.contracts
      Just balance = fmap (view #balance) contract
      Just int = maybeLitWord balance >>= toInt
      out = int2Double int
  in -- trace ("balance: " ++ show out ++ "\ncontracts: " ++ show (Map.keys st.env.contracts))
     out

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

  operation : hevmDecision "AllPlayers" (transactionList 2) ;
  outputs   : transactions ;
  returns   : balance finalState "a";

  inputs    : transactions, globalState;
  feedback  : ;
  operation : fromLensM (uncurry sendAndRun) (const pure) ;
  outputs   : finalState ;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

playerAutomatic =
  [opengame|
  inputs   : ;
  feedback : ;
  :-------:

  operation : hevmDecision "AllPlayers" (transactionList 2) ;
  outputs   : transactions ;
  returns   : balance finalState "a";

  inputs    : transactions;
  feedback  : ;
  operation : fromLensM (sendAndRun') (const pure) ;
  outputs   : finalState ;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

initial :: ST s (VM s)
initial = loadContracts [("Piggybank", "solidity/Withdraw.sol")]

setupAddresses :: [(Expr EAddr, Expr EWord)] -> VM s -> VM s
setupAddresses amounts vm =
  -- generate all the contracts with the given amounts
  let userContracts = fmap (\(addr, amount) -> (addr, set #balance amount emptyContract)) amounts
  -- update the VM state by adding each contract at the corresponding address
  in foldr (\(addr, contract) -> set (#env % #contracts % at addr) (Just contract)) vm userContracts

instance (Apply OpenGames.Engine.Diagnostics.PrintOutput
                (DiagnosticInfoBayesian () EthTransaction)
                [Char]) where
   apply a b = showDiagnosticInfoL [b]

outcome = do
  i <- stToIO initial
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000)] i
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let term :- Nil =  evaluate (playerManual newI) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term newI
  tevaluated <- stToIO t'
  generateOutput (tevaluated :- Nil)

outcomeAutomatic = do
  i <- stToIO initial
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000)] i
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let term :- Nil =  evaluate (playerAutomatic) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term newI
  tevaluated <- stToIO t'
  generateOutput (tevaluated :- Nil)

testExec = do
  evm $ makeTxCall deposit
  -- evm $ makeTxCall (dummyTx 2)
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

interp = do
  i <- stToIO initial
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000)] i
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let storage = preview (#env % #contracts % at withdrawContractAddress %? #storage) newI
  let orig = preview (#env % #contracts % at withdrawContractAddress %? #origStorage) newI
  traceM ("storage: " <> show storage)
  traceM ("origStorage: " <> show orig)
  T.putStrLn (showVM newI)

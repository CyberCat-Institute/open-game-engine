{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.HEVM where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Data.DoubleWord
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import EVM (emptyContract, exec1)
import EVM.Exec
import EVM.Fetch (zero)
import EVM.Format
import EVM.Prelude
import EVM.Stepper (evm, interpret, runFully)
import EVM.TH
import EVM.Types
import GHC.Float
import OpenGames hiding (dependentDecision, fromFunctions, fromLens)
import OpenGames.Engine.Diagnostics
import OpenGames.Engine.HEVMGames
import OpenGames.Preprocessor hiding (Lit)
import Optics.Core (at, over, preview, set, view, (%), (%?), (&), (.~))

$(loadAll [ContractInfo "solidity/Withdraw.sol" "Piggybank" "store"])

deposit :: EthTransaction
deposit = store_deposit userContractAddress 1000 10_000_000

dummyTx :: Word256 -> EthTransaction
dummyTx = store_retrieve userContractAddress 0 100000000

userContractAddress = LitAddr 0x1234

withdrawContractAddress = LitAddr 0x1000

transactionList :: Word256 -> [EthTransaction]
transactionList max = [dummyTx n | n <- [1 .. max]]

playerManual globalState =
  [opengame|
  inputs   : ;
  feedback : ;
  :-------:

  operation : hevmDecision "AllPlayers" (transactionList 2) ;
  outputs   : transactions ;
  returns   : balance finalState userContractAddress;

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
  returns   : balance finalState userContractAddress;

  inputs    : transactions;
  feedback  : ;
  operation : fromLensM sendAndRun' (const pure) ;
  outputs   : finalState ;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

outcome = do
  i <- stToIO initial
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000)] i
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let term :- Nil = evaluate (playerManual newI) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term newI
  tevaluated <- stToIO t'
  generateOutput ([tevaluated] :- Nil)

outcomeAutomatic = do
  i <- stToIO initial
  putStrLn "initial contracts:"
  print $ getAllContracts i
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000),
                             (LitAddr 0x1000, Lit 1000)] i
  putStrLn "setup contracts:"
  print $ getAllContracts newI
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let term :- Nil = evaluate (playerAutomatic) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term newI
  putStrLn "end contracts:"
  print $ getAllContracts newI
  tevaluated <- stToIO t'
  generateOutput ([tevaluated] :- Nil)

testExec = do
  evm $ makeTxCall deposit
  -- evm $ makeTxCall (dummyTx 2)
  runFully

showVM :: VM s -> Text
showVM vm =
  T.unlines
    [ "Contracts:",
      indent 2 . T.unlines . Map.elems $ Map.mapWithKey (\a c -> T.pack (show a) <> " :\n  " <> showContract c) vm.env.contracts,
      -- , "Storage: " <> (formatExpr vm.env.storage)
      --
      "CallValue: " <> (formatExpr vm.state.callvalue),
      "Result: " <> (T.pack $ show vm.result)
    ]

showContract :: Contract -> Text
showContract c =
  T.unlines
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

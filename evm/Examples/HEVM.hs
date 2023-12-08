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
deposit = store_deposit userContractAddress 1000 10000000

userContractAddress = LitAddr 0x1234

withdrawContractAddress = LitAddr 0x1000

dummyTx :: Word256 -> EthTransaction
dummyTx = store_retrieve userContractAddress 0 100000000

transactionList :: Word256 -> [EthTransaction]
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


setupAddresses :: [(Expr EAddr, Expr EWord)] -> VM s -> VM s
setupAddresses amounts vm =
  -- generate all the contracts with the given amounts
  let userContracts = fmap (\(addr, amount) -> (addr, set #balance amount emptyContract)) amounts
   in -- update the VM state by adding each contract at the corresponding address
      foldr (\(addr, contract) -> set (#env % #contracts % at addr) (Just contract)) vm userContracts

instance
  ( Apply
      OpenGames.Engine.Diagnostics.PrintOutput
      (DiagnosticInfoBayesian () EthTransaction)
      [Char]
  )
  where
  apply a b = showDiagnosticInfoL [b]

outcome = do
  i <- stToIO initial
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000)] i
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let term :- Nil = evaluate (playerManual newI) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term newI
  tevaluated <- stToIO t'
  generateOutput (tevaluated :- Nil)

outcomeAutomatic = do
  i <- stToIO initial
  let newI = setupAddresses [(userContractAddress, Lit 1_000_000_000)] i
  newI <- interpret (zero 0 (Just 0)) newI (evm (makeTxCall deposit) >> runFully)
  let term :- Nil = evaluate (playerAutomatic) ((pure (dummyTx 1)) :- Nil) void
  let t' = evalStateT term newI
  tevaluated <- stToIO t'
  generateOutput (tevaluated :- Nil)

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

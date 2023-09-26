{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module EVM.TH where

import Act.Prelude (EthTransaction (..))
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.State.Strict (State, put)
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Text (Text, unpack)
import Data.Text.IO (readFile)
import qualified Data.Tree.Zipper as Zipper
import Data.Vector as Vector (fromList)
import EVM (blankState, initialContract, loadContract, resetState)
import EVM.ABI
import EVM.Exec (exec, run)
import EVM.Expr
import EVM.FeeSchedule
import EVM.Fetch
import EVM.Solidity (solcRuntime)
import EVM.Stepper
import EVM.Transaction (initTx)
import EVM.Types
import GHC.IO.Unsafe
import Language.Haskell.TH.Syntax as TH
import Optics.Core
import Optics.State
import Optics.State.Operators
import Prelude hiding (FilePath, readFile)

-- put this in sttate.callData
-- run it to execute the transaction
-- put more for subsequent calls
-- run more for more results
makeCallData :: EthTransaction -> Expr Buf
makeCallData (EthTransaction _ caller method args _ _) =
  ConcreteBuf $ abiMethod method (AbiTuple (Vector.fromList args))

makeTxCall :: EthTransaction -> EVM ()
makeTxCall tx@(EthTransaction addr caller meth args amt gas) = do
  resetState
  assign (#tx % #isCreate) False
  execState (loadContract addr) <$> get >>= put
  assign (#state % #callvalue) (Lit amt)
  assign (#state % #calldata) (makeCallData tx)
  assign (#state % #caller) (litAddr caller)
  assign (#state % #gas) gas
  -- origin <-
  --     initialContract (RuntimeCode (ConcreteRuntimeCode ""))
  -- let insufficientBal = maybe False
  --   (\b -> b < params.gasprice * (into params.gasCall))
  --   (maybeLitWord origin.balance)
  -- when insufficientBal $ internalError "insufficient balance for gas cost"
  vm <- get
  put $ initTx vm

emptyVM :: [(Addr, ByteString)] -> VM
emptyVM contracts =
  VM
    { result = Nothing,
      state = blankState,
      frames = [],
      env = envForContracts contracts,
      block = emptyBlock,
      tx = emptyTransaction,
      logs = [],
      traces = Zipper.fromForest mempty,
      cache = Cache mempty mempty mempty,
      burned = 0,
      iterations = mempty,
      constraints = [],
      keccakEqs = [],
      allowFFI = True,
      overrideCaller = Nothing
    }
  where
    -- question: Is that a reasonable empty first block?
    emptyBlock :: Block
    emptyBlock =
      Block
        { coinbase = 0,
          timestamp = Lit 0,
          number = 0,
          prevRandao = 0,
          maxCodeSize = 0,
          gaslimit = 0,
          baseFee = 0,
          schedule = berlin -- specifically this, what is it suppsoed to be?
        }
    emptyTransaction :: TxState
    emptyTransaction =
      TxState
        { gasprice = 0,
          gaslimit = 0,
          priorityFee = 0,
          origin = 0,
          toAddr = 0,
          value = Lit 0,
          substate = emptySubState,
          isCreate = True,
          txReversion = mempty
        }
    emptySubState :: SubState
    emptySubState =
      SubState
        { selfdestructs = [],
          touchedAccounts = [],
          accessedAddresses = mempty,
          accessedStorageKeys = mempty,
          refunds = []
        }

    envForContracts :: [(Addr, ByteString)] -> Env
    envForContracts contracts =
      Env
        { contracts = Map.fromList (fmap (fmap bytecodeToContract) contracts),
          chainId = 0,
          storage = EmptyStore,
          origStorage = mempty,
          sha3Crack = mempty
        }

    bytecodeToContract :: ByteString -> Contract
    bytecodeToContract = initialContract . RuntimeCode . ConcreteRuntimeCode

-- setup a new VM state from the list of contracts we are using
loadIntoVM :: [(Addr, ByteString)] -> VM
loadIntoVM contracts = (emptyVM contracts)

-- import a list of contracts as an open game
-- - first we read off all the files and translate them into solidity bytecode
-- - Then we associate each contract to a contract name which
loadEVM :: [(Text, Text)] -> IO VM
loadEVM contracts = do
  files :: [(Text, Text)] <- traverse (\(name, filename) -> (name,) <$> readFile (unpack filename)) (contracts)
  contracts :: [ByteString] <-
    traverse
      ( \(nm, body) -> do
          bytecode <- solcRuntime nm body
          maybe (error "solc failed") pure bytecode
      )
      files
  let bytecodeMap :: [(Addr, ByteString)] = zip [0xabcd ..] contracts
  let newVM = loadIntoVM bytecodeMap
  pure newVM

loadContracts :: [(Text, Text)] -> VM
loadContracts arg = unsafePerformIO $ loadEVM arg

-- TODO: use foundry
thatOneMethod =
  let st = loadContracts [("Neg", "solitidy/Simple.sol")]
      ourTransaction =
        EthTransaction
          0xabcd
          0x1234
          "negate(int256)"
          [AbiInt 256 3]
          100000000
          100000000
      steps = do
        evm (makeTxCall ourTransaction)
        runFully
   in interpret (zero 0 (Just 0)) st steps

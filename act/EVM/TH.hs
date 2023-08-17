{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module EVM.TH where

import Act.Prelude (EthTransaction (..))
import Control.Monad.Trans.State.Strict (State, put)
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Text (Text, unpack)
import Data.Text.IO (readFile)
import qualified Data.Tree.Zipper as Zipper
import Data.Vector as Vector (fromList)
import EVM (blankState, initialContract)
import EVM.ABI
import EVM.FeeSchedule
import EVM.Solidity (solcRuntime)
import EVM.Types
import GHC.IO.Unsafe
import Language.Haskell.TH.Syntax as TH
import Prelude hiding (FilePath, readFile)

-- put this in sttate.callData
-- run it to execute the transaction
-- put more for subsequent calls
-- run more for more results
makeCallData :: EthTransaction -> ByteString
makeCallData (EthTransaction _ method args _ _) =
  abiMethod method (AbiTuple (Vector.fromList args))

sendTransaction :: Contract -> EthTransaction -> VM -> VM
sendTransaction cts tx st = st { frames = [txFrame] }
  where
  txFrame :: Frame
  txFrame = Frame frameCtx frameState

  frameCtx :: FrameContext
  frameCtx = CallContext
      { target        = tx.contract
      , context       = 0
      , offset        = 0
      , size          = 0
      , codehash      = cts.codehash
      , abi           = Nothing
      , calldata      = ConcreteBuf (makeCallData tx)
      , callreversion = (undefined, undefined)
      , subState      = SubState mempty mempty mempty mempty mempty
      }

  frameState :: FrameState
  frameState = FrameState
      { contract     = tx.contract
      , codeContract = undefined
      , code         = cts.contractcode
      , pc           = 0
      , stack        = mempty
      , memory       = undefined
      , memorySize   = 0
      , calldata     = ConcreteBuf (makeCallData tx)
      , callvalue    = Lit 0
      , caller       = Lit 0
      , gas          = tx.gas
      , returndata   = undefined
      , static       = undefined
      }

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
loadIntoVM :: [(Addr, ByteString)] -> State VM ()
loadIntoVM contracts = put (emptyVM contracts)

-- import a list of contracts as an open game
-- - first we read off all the files and translate them into solidity bytecode
-- - Then we associate each contract to a contract name which
loadEVM :: [(Text, Text)] -> IO (State VM ())
loadEVM contracts = do
  files :: [(Text, Text)] <- traverse (\(name, filename) -> (name,) <$> readFile (unpack filename)) (contracts)
  contracts :: [ByteString] <-
    traverse
      ( \(nm, body) -> do
          Just bytecode <- solcRuntime nm body
          pure bytecode
      )
      files
  let bytecodeMap :: [(Addr, ByteString)] = zip [0 ..] contracts
  let newVM = loadIntoVM bytecodeMap
  pure newVM

loadContracts :: [(Text, Text)] -> State VM ()
loadContracts arg = unsafePerformIO $ loadEVM arg

compileTimeLoad :: [(Text, Text)] -> Q [Dec]
compileTimeLoad = undefined

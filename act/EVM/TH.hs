{-# LANGUAGE DuplicateRecordFields, OverloadedLabels, OverloadedRecordDot #-}

module EVM.TH where

import Prelude hiding (FilePath, readFile)
import Data.Text (Text, pack)
import Data.Text.IO
import Data.Map as Map
import Data.ByteString (ByteString)
import Data.Vector  as Vector (fromList)
import Data.Tree.Zipper qualified as Zipper

import Language.Haskell.TH.Syntax as TH
import EVM.ABI
import EVM.Solidity (solcRuntime)
import EVM.Types
import EVM.FeeSchedule
import EVM (blankState, initialContract)
import Act.Prelude (EthTransaction(..))
import Control.Monad.Trans.State.Strict (State, put)

makeCallData :: EthTransaction -> ByteString
makeCallData (EthTransaction _ method args _ _) =
  abiMethod method (AbiTuple (Vector.fromList args))

emptyVM :: [(Addr, ByteString)] -> VM
emptyVM contracts = VM
  { result         = Nothing
  , state          = blankState
  , frames         = []
  , env            = envForContracts contracts
  , block          = emptyBlock
  , tx             = emptyTransaction
  , logs           = []
  , traces         = Zipper.fromForest mempty
  , cache          = Cache mempty mempty mempty
  , burned         = 0
  , iterations     = mempty
  , constraints    = []
  , keccakEqs      = []
  , allowFFI       = True
  , overrideCaller = Nothing
  }
  where
  -- question: Is that a reasonable empty first block?
  emptyBlock :: Block
  emptyBlock = Block
    { coinbase    = 0
    , timestamp   = Lit 0
    , number      = 0
    , prevRandao  = 0
    , maxCodeSize = 0
    , gaslimit    = 0
    , baseFee     = 0
    , schedule    = berlin -- specifically this, what is it suppsoed to be?
    }
  emptyTransaction :: TxState
  emptyTransaction = TxState
    { gasprice = 0
    , gaslimit = 0
    , priorityFee = 0
    , origin = 0
    , toAddr = 0
    , value = Lit 0
    , substate = emptySubState
    , isCreate = True
    , txReversion = mempty
    }
  emptySubState :: SubState
  emptySubState = SubState
    { selfdestructs = []
    , touchedAccounts = []
    , accessedAddresses = mempty
    , accessedStorageKeys = mempty
    , refunds = []
    }

  envForContracts :: [(Addr, ByteString)] -> Env
  envForContracts contracts = Env
    { contracts = Map.fromList (fmap (fmap bytecodeToContract) contracts)
    , chainId = 0
    , storage = EmptyStore
    , origStorage = mempty
    , sha3Crack = mempty
    }

  bytecodeToContract :: ByteString -> Contract
  bytecodeToContract = initialContract . RuntimeCode . ConcreteRuntimeCode

-- setup a new VM state from the list of contracts we are using
loadContracts :: [(Addr, ByteString)] -> State VM ()
loadContracts contracts =
  put (emptyVM contracts)

-- import a list of contracts as an open game
-- - first we read off all the files and translate them into solidity bytecode
-- - Then we associate each contract to a contract name which
evm2OG :: Map String [Text] -> IO (State VM ())
evm2OG contractMap = do
  files :: [(Text, Text)] <- traverse (\filename -> (pack filename,) <$> readFile filename) (keys contractMap)
  contracts :: [ByteString] <- traverse (\(nm, body) -> do
      Just bytecode <- solcRuntime nm body
      pure bytecode) files
  let bytecodeMap :: [(Addr, ByteString)] = zip [0 .. ] contracts
  let newVM = loadContracts bytecodeMap
  pure newVM


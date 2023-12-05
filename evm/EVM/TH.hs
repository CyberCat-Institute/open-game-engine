{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module EVM.TH where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.State.Strict (State, put)
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Text (Text, pack, unpack, intercalate, toLower)
import Data.Text.IO (readFile)
import qualified Data.Tree.Zipper as Zipper
import Data.Vector as Vector (fromList)
import Debug.Trace
import EVM (blankState, initialContract, loadContract, resetState)
import EVM.Exec (exec, run)
import EVM.Expr
import EVM.FeeSchedule
import EVM.Fetch
import EVM.Prelude
import EVM.Solidity (solcRuntime, readStdJSON, solidity', Contracts(..), SolcContract(..), Method(..))
import EVM.Stepper
import EVM.Transaction (initTx)
import EVM.Types
import GHC.IO.Unsafe
import GHC.ST
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

makeTxCall :: EthTransaction -> EVM s ()
makeTxCall tx@(EthTransaction addr caller meth args amt gas) = do
  resetState
  assign (#tx % #isCreate) False
  modify (execState (loadContract addr))
  assign (#state % #callvalue) (Lit amt)
  assign (#state % #calldata) (makeCallData tx)
  assign (#state % #caller) (caller)
  assign (#state % #gas) gas
  modify initTx

loadIntoVM :: [(Expr EAddr, ByteString)] -> ST s (VM s)
loadIntoVM contracts = do
  blankSt <- blankState
  pure $
    VM
      { result = Nothing,
        state = blankSt,
        frames = [],
        env = envForContracts contracts,
        block = emptyBlock,
        tx = emptyTransaction,
        logs = [],
        traces = Zipper.fromForest mempty,
        cache = Cache mempty mempty,
        burned = 0,
        iterations = mempty,
        constraints = [],
        keccakEqs = [],
        config =
          RuntimeConfig
            True
            Nothing
            EmptyBase
      }
  where
    -- question: Is that a reasonable empty first block?
    emptyBlock :: Block
    emptyBlock =
      Block
        { coinbase = LitAddr 0,
          timestamp = Lit 0,
          number = 0,
          prevRandao = 0,
          maxCodeSize = 0,
          gaslimit = 0,
          baseFee = 0,
          schedule = feeSchedule
        }
    emptyTransaction :: TxState
    emptyTransaction =
      TxState
        { gasprice = 0,
          gaslimit = 0,
          priorityFee = 0,
          origin = LitAddr 0,
          toAddr = LitAddr 0,
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

    envForContracts :: [(Expr EAddr, ByteString)] -> Env
    envForContracts contracts =
      Env
        { contracts = Map.fromList (fmap (fmap bytecodeToContract) contracts),
          chainId = 0
          -- storage = EmptyStore,
          -- origStorage = mempty
        }

    bytecodeToContract :: ByteString -> Contract
    bytecodeToContract = initialContract . RuntimeCode . ConcreteRuntimeCode

-- import a list of contracts as an open game
-- - first we read off all the files and translate them into solidity bytecode
-- - Then we associate each contract to a contract name which
loadEVM :: [(Text, Text)] -> IO (ST s (VM s))
loadEVM contracts = do
  files :: [(Text, Text)] <- traverse (\(name, filename) -> (name,) <$> readFile (unpack filename)) (contracts)
  contracts :: [ByteString] <-
    traverse
      ( \(nm, body) -> do
          bytecode <- solcRuntime nm body
          maybe (error "solc failed") pure bytecode
      )
      files
  let bytecodeMap :: [(Expr EAddr, ByteString)] = zip (fmap LitAddr [0xabcd ..]) contracts
  let newVM = loadIntoVM bytecodeMap
  pure newVM

int :: Int -> Exp
int = LitE . IntegerL . toInteger

constructorExprForType :: AbiType -> Name -> Exp
constructorExprForType (AbiUIntType w) = ((ConE (mkName "AbiUInt") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiIntType w) = ((ConE (mkName "AbiInt") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiAddressType) = (ConE (mkName "AbiAddress") `AppE`) . VarE
constructorExprForType (AbiBoolType) = (ConE (mkName "AbiBool") `AppE`) . VarE
constructorExprForType (AbiBytesType w) = ((ConE (mkName "AbiBytes") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiBytesDynamicType) = (ConE (mkName "AbiBytesDynamic") `AppE`) . VarE
constructorExprForType (AbiStringType) = (ConE (mkName "AbiString") `AppE`) . VarE
constructorExprForType (AbiArrayDynamicType ty) = error "arrays unsupported"
constructorExprForType (AbiArrayType size ty) = error "arrays unsuppported"
constructorExprForType (AbiTupleType types) = error "tuples unsupported" -- ConE (mkName "AbiTuple") [] [VarP (mkName name)]
constructorExprForType (AbiFunctionType) = error "functions unsupported"

loadContracts :: [(Text, Text)] -> ST s (VM s)
loadContracts arg = unsafePerformIO $ loadEVM arg

loadABI :: Text -> Text -> Q [Dec]
loadABI contractFilename contractName = do
  file <- runIO $ readFile (unpack contractFilename)
  (json, path) <- runIO $ solidity' file
  case readStdJSON json of
    Nothing -> error ("could not read json file: " <> show json)
    Just (Contracts sol, a, b) ->
        case Map.lookup (path <> ":" <> contractName) sol of
          Nothing -> error "failed looking up contract"
          Just contract -> let methods = Map.elems $ contract.abiMap
                            in traverse generateTxFactory methods
  where
  printArg :: (Text, AbiType) -> Text
  printArg (n, ty) = n <> ": " <> pack (show ty)

  pat = VarP . mkName

  generateTxFactory :: Method -> Q Dec
  generateTxFactory (Method _ args name sig _) = do
    runIO $ print ("arguments for method " <> name <> ":" <> pack (show args))
    let signatureString :: Q Exp = pure $ LitE $ StringL $ unpack sig
    let argExp :: Q Exp = pure $ ListE $ fmap (\(nm, ty) -> constructorExprForType ty (mkName $ unpack nm)) args
    let patterns :: [Pat] = fmap (VarP . mkName . unpack . fst) args
    body <- [e| EthTransaction src tgt
                  $(signatureString)
                  $(argExp)
                  amt gas|]
    pure $ FunD (mkName (unpack (toLower contractName <> "_" <> name)))
                [Clause ([pat "src", pat "tgt", pat "amt", pat "gas"]
                        ++ patterns)
                        (NormalB body) []]

-- TODO: use foundry
thatOneMethod =
  let st = loadContracts [("Neg", "solidity/Simple.sol")]
      ourTransaction =
        EthTransaction
          (LitAddr 0xabcd)
          (LitAddr 0x1234)
          "negate(int256)"
          [AbiInt 256 3]
          100000000
          100000000
      steps = do
        evm (makeTxCall ourTransaction)
        runFully
   in interpret (zero 0 (Just 0)) undefined steps

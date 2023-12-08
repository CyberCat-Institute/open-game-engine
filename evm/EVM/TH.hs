{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module EVM.TH where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.State.Strict (State, put)
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Text (Text, pack, unpack, intercalate, toLower)
import Data.Text.IO (readFile)
import qualified Data.Tree.Zipper as Zipper
import Data.Vector as Vector (fromList)
import Debug.Trace
import EVM (blankState, initialContract, loadContract, resetState, exec1)
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

data ContractInfo = ContractInfo
    { file :: Text
    , name :: Text
    , boundName :: Text
    }

-- loadContracts :: [ContractInfo] -> ST s (VM s)
-- loadContracts arg = unsafePerformIO $ loadEVM arg

-- import a list of contracts as an open game
-- - first we read off all the files and translate them into solidity bytecode
-- - Then we associate each contract to a contract name which
-- loadEVM :: [ContractInfo] -> IO (ST s (VM s))
-- loadEVM contracts = do
--   files :: [ContractInfo] <-
--       traverse (\(ContractInfo filename name bound) ->
--                   ContractInfo <$> readFile (unpack filename)
--                                <*> pure name
--                                <*> pure bound) contracts
--   contracts :: [(Text, Text, ByteString)] <-
--     traverse
--       ( \(ContractInfo fileBody nm bound) -> do
--           bytecode <- solcRuntime nm body
--           maybe (error "solc failed") (pure . (nm, bound,)) bytecode
--       )
--       files
--   let bytecodeMap :: [(Expr EAddr, ByteString)] = zip (fmap LitAddr [0xabcd ..]) contracts
--   let newVM = loadIntoVM bytecodeMap
--   pure newVM

pat = VarP . mkName

generateTxFactory :: Method -> Integer -> Text -> Q Dec
generateTxFactory (Method _ args name sig _) addr boundName = do
  runIO $ print ("arguments for method " <> name <> ":" <> pack (show args))
  let signatureString :: Q Exp = pure $ LitE $ StringL $ unpack sig
  let argExp :: Q Exp = pure $ ListE $ fmap (\(nm, ty) -> constructorExprForType ty (mkName $ unpack nm)) args
  let patterns :: [Pat] = fmap (VarP . mkName . unpack . fst) args
  let contractAddress :: Q Exp = pure $ AppE (ConE (mkName "LitAddr")) (LitE (IntegerL addr))
  body <- [e| EthTransaction
                $(contractAddress)
                src
                $(signatureString)
                $(argExp)
                amt gas|]
  pure $ FunD (mkName (unpack (toLower boundName <> "_" <> name)))
              [Clause ([pat "src", pat "amt", pat "gas"]
                      ++ patterns)
                      (NormalB body) []]

instance Lift (Addr) where
  lift (Addr word) = let v = toInteger word in [e| fromInteger v |]

instance Lift (Expr 'EAddr) where
  lift (LitAddr a) = [e| LitAddr (fromInteger a) |]

loadAll :: [ContractInfo] -> Q [Dec]
loadAll contracts = do
  cs <- runIO $ traverse loadSolcInfo contracts
  let indexed = zip [0x1000 ..] cs
  methods <- traverse (\(ix, (_, bn, _, m)) -> traverse (\m' -> generateTxFactory m' ix bn) m) indexed
  let allMethods = concat methods
  let contractMap = fmap (\(ix, (_, _, b, _)) -> (LitAddr (fromInteger ix), b)) indexed
  init <- [d|
      initial :: ST s (VM s)
      initial = loadIntoVM contractMap
       |]
  pure (init ++ allMethods)

loadSolcInfo :: ContractInfo -> IO (Text, Text, ByteString, [Method])
loadSolcInfo (ContractInfo contractFilename contractName boundName) = do
  file <- readFile (unpack contractFilename)
  solRes <- solidity' file
  let (bytecode, methods) = getSolcResults solRes
  pure (contractName, boundName, bytecode, methods)
  where

  getSolcResults :: (Text, Text) -> (ByteString, [Method])
  getSolcResults (json, path) = case readStdJSON json of
    Nothing -> error ("could not read json file: " <> show json)
    Just (Contracts sol, a, b) ->
        case Map.lookup (path <> ":" <> contractName) sol of
          Nothing -> error "failed looking up contract"
          Just contract -> let methods = Map.elems $ contract.abiMap
                            in (contract.runtimeCode, methods)

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
sendAndRun ::
  EthTransaction ->
  VM RealWorld ->
  EVM RealWorld (VM RealWorld)
sendAndRun tx st = do
  put st
  sendAndRun' tx
  get



-- TODO: use foundry
-- thatOneMethod =
--   let st = loadContracts [ContractInfo "solidity/Simple.sol" "Neg" "test"]
--       ourTransaction =
--         EthTransaction
--           (LitAddr 0xabcd)
--           (LitAddr 0x1234)
--           "negate(int256)"
--           [AbiInt 256 3]
--           100000000
--           100000000
--       steps = do
--         evm (makeTxCall ourTransaction)
--         runFully
--    in interpret (zero 0 (Just 0)) undefined steps

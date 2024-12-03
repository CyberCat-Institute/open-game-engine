{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveLift #-}

module EVM.TH (sendAndRun, sendAndRunAll, sendAndRun', makeTxCall, balance, loadAll
              , ContractFileInfo, mkContractFileInfo, ContractInfo, mkContractInfo, AbiValue (..), Expr (..), stToIO, setupAddresses, getAllContracts) where

import Control.Monad.ST
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.State.Strict (State, put)
import Data.ByteString (ByteString)
import Data.Map as Map
import Data.Text (Text, intercalate, pack, toLower, unpack)
import Data.Text.IO (readFile)
import Data.Maybe (fromMaybe)
import qualified Data.Tree.Zipper as Zipper
import Data.Vector as Vector (Vector, fromList, toList)
import EVM (blankState, emptyContract, exec1, initialContract, loadContract, resetState)
import EVM.Exec (exec, run)
import EVM.Expr
import EVM.FeeSchedule
import EVM.Fetch
import EVM.Prelude
import EVM.Solidity (Contracts (..), Method (..), SolcContract (..), readStdJSON, solcRuntime, solidity, solc, Language(..))
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

makeTxCall :: EthTransaction -> EVM Concrete s ()
makeTxCall tx@(EthTransaction addr caller meth args amt gas) = do
  resetState
  assign (#tx % #isCreate) False
  modify (execState (loadContract addr))
  assign (#state % #callvalue) (Lit amt)
  assign (#state % #calldata) (makeCallData tx)
  assign (#state % #caller) (caller)
  assign (#state % #gas) gas
  modify initTx

loadIntoVM :: [(Expr EAddr, ByteString)] -> ST s (VM Concrete s)
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
        config =
          RuntimeConfig
            True
            Nothing
            False
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
          subState = emptySubState,
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

instance Lift a => Lift (Vector a) where
  lift vec = do ll <- traverse lift (Vector.toList vec)
                let gg = ListE ll
                [| Vector.fromList $( pure gg ) |]

instance Lift AbiType where
  lift (AbiUIntType n)          = [| AbiUIntType n |]
  lift (AbiIntType n)           = [| AbiIntType n |]
  lift AbiAddressType           = [| AbiAddressType |]
  lift AbiBoolType              = [| AbiBoolType |]
  lift (AbiBytesType n)         = [| AbiBytesType n |]
  lift AbiBytesDynamicType      = [| AbiBytesDynamicType |]
  lift AbiStringType            = [| AbiStringType |]
  lift (AbiArrayDynamicType ty) = [| AbiArrayDynamicType ty |]
  lift (AbiArrayType ty arr)    = [| AbiArrayType ty arr |]
  lift (AbiTupleType tys)       = [| AbiTupleType tys |]
  lift AbiFunctionType          = [| AbiFunctionType |]

constructorExprForType :: Quote m => AbiType -> Name -> m Exp
constructorExprForType (AbiUIntType w)  = pure . ((ConE (mkName "AbiUInt") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiIntType w)   = pure . ((ConE (mkName "AbiInt") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiAddressType) = pure . (ConE (mkName "AbiAddress") `AppE`) . VarE
constructorExprForType (AbiBoolType)    = pure . (ConE (mkName "AbiBool") `AppE`) . VarE
constructorExprForType (AbiBytesType w) = pure . ((ConE (mkName "AbiBytes") `AppE` int w) `AppE`) . VarE
constructorExprForType (AbiBytesDynamicType) = pure . (ConE (mkName "AbiBytesDynamic") `AppE`) . VarE
constructorExprForType (AbiStringType) = pure . (ConE (mkName "AbiString") `AppE`) . VarE
constructorExprForType (AbiArrayDynamicType ty) = \nm ->  [|AbiArrayDynamic ty $(pure (VarE nm))|]
constructorExprForType (AbiArrayType size ty) = error "arrays unsuppported"
constructorExprForType (AbiTupleType types) = error "tuples unsupported" -- ConE (mkName "AbiTuple") [] [VarP (mkName name)]
constructorExprForType (AbiFunctionType) = error "functions unsupported"

data ContractInfo' a = ContractInfo' { name :: Text, boundName :: Text, payload :: a}
  deriving Functor

type ContractInfo = ContractInfo' ()

mkContractInfo :: Text -> Text -> ContractInfo
mkContractInfo name boundName = ContractInfo' name boundName ()

data ContractFileInfo' a = ContractFileInfo'
  { file :: Text,
    modules :: [ContractInfo' a]
  }

type ContractFileInfo = ContractFileInfo' ()

mkContractFileInfo :: Text -> [ContractInfo] -> ContractFileInfo
mkContractFileInfo = ContractFileInfo'

pat = VarP . mkName

generateTxFactory :: Method -> Integer -> Text -> Q Dec
generateTxFactory (Method _ args name sig _) addr contractName = do
  runIO $ print ("arguments for method " <> name <> ":" <> pack (show args))
  let signatureString :: Q Exp = pure $ LitE $ StringL $ unpack sig
  let argExp :: Q Exp = ListE <$> traverse (\(nm, ty) -> constructorExprForType ty (mkName $ unpack nm)) args
  let patterns :: [Pat] = fmap (VarP . mkName . unpack . fst) args
  let contractAddress :: Q Exp = pure $ AppE (ConE (mkName "LitAddr")) (LitE (IntegerL addr))
  body <-
    [e|
      EthTransaction
        $(contractAddress)
        src
        $(signatureString)
        $(argExp)
        amt
        gas
      |]
  pure $
    FunD
      (mkName (unpack (toLower contractName <> "_" <> name)))
      [ Clause
          ( [pat "src", pat "amt", pat "gas"]
              ++ patterns
          )
          (NormalB body)
          []
      ]

instance Lift (Addr) where
  lift (Addr word) = let v = toInteger word in [e|fromInteger v|]

instance Lift (Expr 'EAddr) where
  lift (LitAddr a) = [e|LitAddr (fromInteger a)|]

instance Num (Expr 'EAddr) where
  fromInteger i = LitAddr (fromInteger i)

loadAll :: [ContractFileInfo] -> Q [Dec]
loadAll contracts = do
  allContracts <- runIO $ traverse loadSolcInfo contracts
  let allContractsHash = zip [ 0x1000.. ] (concat allContracts)
  methods <- generateDefsForMethods allContractsHash
  let contractMap = generateContractMap allContractsHash
  contractNames <- traverse (\(addr, ContractInfo' nm bn con) -> contractName bn addr) allContractsHash
  -- traverse (\(ix, ContractFileInfo _ nm) -> contractName nm ix) (zip [0x1000 ..] contracts)
  init <-
    [d|
      initial = loadIntoVM contractMap
      |]
  pure (init ++ methods ++ contractNames)
  where
                        -- Address, Contract name, Bound name
    generateContractMap :: [(Integer, ContractInfo' SolcContract)] -> [(Integer, ByteString)]
    generateContractMap = fmap (\(i, contract) -> (i, contract.payload.runtimeCode))
                        -- Address, Contract name, Bound name
    generateDefsForMethods :: [(Integer, ContractInfo' SolcContract)] -> Q [Dec]
    generateDefsForMethods [] = pure []
    generateDefsForMethods ((hash, ContractInfo' name boundName contract) : xs) = do
      let methods = Map.elems contract.abiMap
      traverse (\x -> generateTxFactory x hash boundName) methods

    contractName :: Text -> Integer -> Q Dec
    contractName binder value = do
      let nm = mkName (unpack (binder <> "_contract"))
      let value' = LitAddr (fromInteger value)
      addr <- [e|value'|]
      pure (ValD (VarP nm) (NormalB addr) [])

loadSolcInfo :: ContractFileInfo -> IO [ContractInfo' SolcContract]
loadSolcInfo (ContractFileInfo' contractFilename modules) = do
  file <- readFile (unpack contractFilename)
  json <- solc Solidity file
  let (Contracts sol, _, _) = fromMaybe (error ("canot read json:" ++ unpack json)) (readStdJSON json)
  let retrievedMap = fmap (\mod -> fmap (\() -> Map.lookup ("hevm.sol:" <> mod.name) sol) mod) modules
  emitMissing retrievedMap
  where
    emitMissing :: Show a => [ContractInfo' (Maybe a)] -> IO [ContractInfo' a]
    emitMissing [] = pure []
    emitMissing (ContractInfo' t s Nothing : xs) = putStrLn ("contract " ++ show t ++ "is missing") >> emitMissing xs
    emitMissing (ContractInfo' t s (Just x) : xs) = (ContractInfo' t s x :) <$> emitMissing xs

run' :: EVM Concrete s (VM Concrete s)
run' = do
  vm <- get
  case vm.result of
    Nothing -> exec1 >> run'
    Just (HandleEffect (Query (PleaseAskSMT (Lit c) _ cont))) ->
      undefined -- cont (Case (c > 0)) >> run'
    Just (VMFailure y) -> pure vm
    Just (VMSuccess y) -> pure vm

-- send and run a transaction on the EVM Concrete state
sendAndRun' :: EthTransaction -> EVM Concrete RealWorld (VM Concrete RealWorld)
sendAndRun' tx = do
  EVM.TH.makeTxCall tx
  vm <- run'
  pure vm

sendAndRunAll :: [EthTransaction] -> EVM Concrete RealWorld (VM Concrete RealWorld)
sendAndRunAll [transaction] = sendAndRun' transaction
sendAndRunAll (tx : ts) = do
  EVM.TH.makeTxCall tx
  vm <- run'
  sendAndRunAll ts

-- exectute the EVM Concrete state in IO
sendAndRun ::
  EthTransaction ->
  VM Concrete RealWorld ->
  EVM Concrete RealWorld (VM Concrete RealWorld)
sendAndRun tx st = do
  put st
  sendAndRun' tx
  get

adjustOrAdd :: (Ord k) => (v -> v) -> v -> k -> Map.Map k v -> Map.Map k v
adjustOrAdd f def = alter (Just . maybe def f)

setupAddresses :: [(Expr EAddr, Expr EWord)] -> VM Concrete s -> VM Concrete s
setupAddresses amounts =
  over (#env % #contracts) (updateContractMap amounts)
  where
    updateContractMap ::
      [(Expr EAddr, Expr EWord)] ->
      Map.Map (Expr EAddr) Contract ->
      Map.Map (Expr EAddr) Contract
    updateContractMap [] x = x
    updateContractMap ((addr, amount) : cs) map =
      let map' = adjustOrAdd (set #balance amount) (set #balance amount emptyContract) addr map
       in updateContractMap cs map'

    createNew (addr, amount) = (addr, set #balance amount emptyContract)

    updateContractState :: (Expr EAddr, Contract) -> VM Concrete s -> VM Concrete s
    updateContractState (addr, contract) = set (#env % #contracts % at addr) (Just contract)

getAllContracts :: VM Concrete s -> [(Expr EAddr, Expr EWord)]
getAllContracts vm =
  let contracts = Map.toList $ view (#env % #contracts) vm
      contractsAmounts = fmap (fmap (view #balance)) contracts
   in contractsAmounts

balance :: VM Concrete s -> Expr EAddr -> W256
balance st addr =
  let contract = Map.lookup addr st.env.contracts
      Just balance = fmap (view #balance) contract
      Just int = maybeLitWord balance
   in int

-- TODO: use foundry
-- thatOneMethod =
--   let st = loadContracts [ContractFileInfo "solidity/Simple.sol" "Neg" "test"]
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

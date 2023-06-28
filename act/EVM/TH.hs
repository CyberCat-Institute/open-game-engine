{-# LANGUAGE OverloadedLabels, OverloadedRecordDot #-}

module EVM.TH where

import Prelude hiding (readFile)
import Data.Text (Text, pack)
import Data.Text.IO
import Data.ByteString (ByteString)
import Data.Vector  as Vector (Vector, fromList)
import Language.Haskell.TH.Syntax as TH
import EVM.ABI
import EVM.Solidity
import EVM.Exec
import EVM.UnitTest
import EVM.Types
import Act.Prelude (EthTransaction(..))
import Control.Monad.Trans.State.Strict (get, State)

import Optics.Core hiding (elements)
import Optics.State
import Optics.State.Operators
import Optics.Zoom


makeCallData :: EthTransaction -> ByteString
makeCallData (EthTransaction _ method args _ _) =
  abiMethod method (AbiTuple (Vector.fromList args))

paramsFromTx :: EthTransaction -> TestVMParams
paramsFromTx = undefined

injectTransaction :: EthTransaction -> State VM ()
injectTransaction trans = do
  makeTxCall (paramsFromTx trans) (ConcreteBuf (makeCallData trans), [])

runState :: [EthTransaction] -> State VM VM
runState tr = traverse injectTransaction tr >> run

loadContracts :: [(Addr, ByteString)] -> State VM ()
loadContracts = undefined

-- - we get an initial vm by calling it's constructor and setting most its
--   fields as empty
-- - env is from VM
-- - change the env to contain all contracts
--


-- import a list of contracts as an open game
evm2OG :: [String] -> Q [Dec]
evm2OG files = do
  files :: [(Text, Text)] <- runIO $ traverse (\filename -> (pack filename,) <$> readFile filename) files
  -- bytecode <- runIO $ loadContracts (??/a(uncurry solcRuntime) files
  pure []

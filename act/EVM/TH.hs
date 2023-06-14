module EVM.TH where

import Prelude hiding (readFile)
import Data.Text (Text, pack)
import Data.Text.IO
import Language.Haskell.TH.Syntax as TH
import EVM.Solidity
import EVM.Exec
import EVM.Types
import Act.Prelude
import Control.Monad.Trans.State.Strict (get, State)

injectTransaction :: Transaction -> State VM ()
injectTransaction = undefined

runState :: [Transaction] -> State VM VM
runState tr = traverse injectTransaction tr >> run

loadContract :: ByteString -> State VM ()
loadContract = undefined

-- import a list of contracts as an open game
evm2OG :: [String] -> Q [Dec]
evm2OG files = do
  files :: [(Text, Text)] <- runIO $ traverse (\filename -> (pack filename,) <$> readFile filename) files
  bytecode <- runIO $ traverse (uncurry solcRuntime) files
  pure []

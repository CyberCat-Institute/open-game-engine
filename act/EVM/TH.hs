module EVM.TH where

import Prelude hiding (readFile)
import Data.Text (Text, pack)
import Data.Text.IO
import Language.Haskell.TH.Syntax as TH
import EVM.Solidity

-- import a list of contracts as an open game
evm2OG :: [String] -> Q [Dec]
evm2OG files = do
  files :: [(Text, Text)] <- runIO $ traverse (\filename -> (pack filename,) <$> readFile filename) files
  bytecode <- runIO $ traverse (uncurry solcRuntime) files
  pure []

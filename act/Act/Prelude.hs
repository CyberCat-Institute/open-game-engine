module Act.Prelude (Transaction(..), AbiType(..)) where

import EVM.Solidity (SlotType)
import EVM.ABI (AbiType(..))

data Transaction = Transaction
  { contract :: String
  , method :: String
  , arguments :: [AbiType]
  }
  deriving (Eq, Show)

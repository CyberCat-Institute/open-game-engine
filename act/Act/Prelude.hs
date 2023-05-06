module Act.Prelude (Transaction (..), AbiType (..)) where

import EVM.ABI (AbiType (..))
import EVM.Solidity (SlotType)

data Transaction = Transaction
  { contract :: String,
    method :: String,
    arguments :: [AbiType]
  }
  deriving (Eq, Show, Ord)

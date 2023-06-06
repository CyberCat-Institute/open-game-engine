module Act.Prelude (Word256, Transaction (..), AbiType (..), AbiValue(..)) where

import EVM.ABI (AbiType (..), AbiValue (..))
import EVM.Solidity (SlotType)
import Data.DoubleWord (Word256, Int256, signedWord)

data Transaction = Transaction
  { contract :: String,
    method :: String,
    arguments :: [AbiValue]
  }
  deriving (Eq, Show, Ord)

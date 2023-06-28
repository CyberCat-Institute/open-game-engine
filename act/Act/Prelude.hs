{-# LANGUAGE DuplicateRecordFields
    , NoFieldSelectors
    , OverloadedRecordDot
 #-}
module Act.Prelude (Word256, EthTransaction(..), Transaction (..), AbiType (..), AbiValue(..)) where

import EVM.ABI (AbiType (..), AbiValue (..))
import EVM.Solidity (SlotType)
import EVM.Types (Addr, W256)

import Data.Text
import Data.DoubleWord (Word256)

data Transaction = Transaction
  { contract :: String,
    method :: String,
    arguments :: [AbiValue]
  }
  deriving (Eq, Show, Ord)

data EthTransaction = EthTransaction
  { contract :: Addr,
    method :: Text,
    arguments :: [AbiValue],
    ethAmt :: W256,
    gas :: W256
  }
  deriving (Eq, Show, Ord)

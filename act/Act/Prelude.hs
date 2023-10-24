{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Act.Prelude (Word256, EthTransaction (..), Transaction (..), AbiType (..), AbiValue (..)) where

import Data.DoubleWord (Word256)
import Data.Text
import EVM.ABI (AbiType (..), AbiValue (..))
import EVM.Types (Addr, W256)
import GHC.Word

data Transaction = Transaction
  { contract :: String,
    method :: String,
    arguments :: [AbiValue]
  }
  deriving (Eq, Show, Ord)


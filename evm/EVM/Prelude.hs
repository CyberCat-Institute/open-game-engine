{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module EVM.Prelude (Word256, EthTransaction (..), AbiType (..), AbiValue (..)) where

import Data.DoubleWord (Word256)
import Data.Text
import EVM.ABI (AbiType (..), AbiValue (..))
import EVM.Types (Addr, EType (..), Expr (..), W256)
import GHC.Word

data EthTransaction = EthTransaction
  { contract :: Expr EAddr,
    caller :: Expr EAddr,
    method :: Text,
    arguments :: [AbiValue],
    ethAmt :: W256,
    gas :: Word64
  }
  deriving (Eq, Show, Ord)

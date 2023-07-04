{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Examples.EVM where

import EVM.TH
import Data.Text

$(loadContracts [ ("token1", "erc20.sol")
                , ("token2", "erc20.sol")
                , ("amm", "amm.sol")
                ])


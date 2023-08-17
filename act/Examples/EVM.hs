{-# LANGUAGE OverloadedStrings #-}

module Examples.EVM where

import EVM.TH

-- todo:
-- - Lending platform, look for aave.  aave.sol ??
-- - Implement state into the open game
-- - Obtain amm.sol and erc20.sol
-- - test arbitrage strategy
blockchainState =
  loadContracts
    [ ("token1", "ERC20.sol"),
      ("token2", "ERC20.sol"),
      ("amm", "AMM.sol")
    ]

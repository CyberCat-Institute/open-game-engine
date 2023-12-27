{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Lido where

import EVM.TH

-- lido = loadContracts [("LidoExecutionLayerRewardsVault", "solidity/LidoReward.sol")]
-- lido = [loadContract| file : solidity/LidoReward.sol, contract : LidoExecutionLayerRewardsVault, name : lido]
--

-- $(loadABI "solidity/LidoReward.sol" "LidoExecutionLayerRewardsVault")

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Lido where

import EVM.TH

$(loadAll [mkContractFileInfo "DualGovernance.sol" [mkContractInfo "DualGovernance" "dualgov"]])
-- lido = loadContracts [("EmergencyProtectedTimelockModel", "solidity/EmergencyProtectedTimelockModel.sol")]
-- lido = [loadContract| file : solidity/LidoReward.sol, contract : LidoExecutionLayerRewardsVault, name : lido]

-- lido = loadContracts [("LidoExecutionLayerRewardsVault", "solidity/LidoReward.sol")]
-- lido = [loadContract| file : solidity/LidoReward.sol, contract : LidoExecutionLayerRewardsVault, name : lido]
--

-- $(loadAll [ContractInfo "solidity/EmergencyProtectedTimelockModel.sol" "TimelockModel" "model"])

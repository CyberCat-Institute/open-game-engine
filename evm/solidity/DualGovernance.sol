// SPDX-License-Identifier: MIT
pragma solidity 0.8.26;

import {Duration} from "./types/Duration.sol";
import {Timestamp} from "./types/Timestamp.sol";
import {ITimelock} from "./interfaces/ITimelock.sol";
import {IResealManager} from "./interfaces/IResealManager.sol";

import {IStETH} from "./interfaces/IStETH.sol";
import {IWstETH} from "./interfaces/IWstETH.sol";
import {IWithdrawalQueue} from "./interfaces/IWithdrawalQueue.sol";
import {IDualGovernance} from "./interfaces/IDualGovernance.sol";
import {IResealManager} from "./interfaces/IResealManager.sol";

import {Proposers} from "./libraries/Proposers.sol";
import {Tiebreaker} from "./libraries/Tiebreaker.sol";
import {ExternalCall} from "./libraries/ExternalCalls.sol";
import {State, DualGovernanceStateMachine} from "./libraries/DualGovernanceStateMachine.sol";
import {IDualGovernanceConfigProvider} from "./DualGovernanceConfigProvider.sol";

import {Escrow} from "./Escrow.sol";

contract DualGovernance is IDualGovernance {
    using Proposers for Proposers.Context;
    using Tiebreaker for Tiebreaker.Context;
    using DualGovernanceStateMachine for DualGovernanceStateMachine.Context;

    // ---
    // Errors
    // ---

    error NotAdminProposer();
    error UnownedAdminExecutor();
    error CallerIsNotResealCommittee(address caller);
    error CallerIsNotAdminExecutor(address caller);
    error InvalidConfigProvider(IDualGovernanceConfigProvider configProvider);
    error ProposalSubmissionBlocked();
    error ProposalSchedulingBlocked(uint256 proposalId);
    error ResealIsNotAllowedInNormalState();

    // ---
    // Events
    // ---

    event CancelAllPendingProposalsSkipped();
    event CancelAllPendingProposalsExecuted();
    event EscrowMasterCopyDeployed(address escrowMasterCopy);
    event ConfigProviderSet(IDualGovernanceConfigProvider newConfigProvider);

    // ---
    // Tiebreaker Sanity Check Param Immutables
    // ---

    struct SanityCheckParams {
        uint256 minWithdrawalsBatchSize;
        Duration minTiebreakerActivationTimeout;
        Duration maxTiebreakerActivationTimeout;
        uint256 maxSealableWithdrawalBlockersCount;
    }

    Duration public immutable MIN_TIEBREAKER_ACTIVATION_TIMEOUT;
    Duration public immutable MAX_TIEBREAKER_ACTIVATION_TIMEOUT;
    uint256 public immutable MAX_SEALABLE_WITHDRAWAL_BLOCKERS_COUNT;

    // ---
    // External Parts Immutables

    struct ExternalDependencies {
        IStETH stETH;
        IWstETH wstETH;
        IWithdrawalQueue withdrawalQueue;
        ITimelock timelock;
        IResealManager resealManager;
        IDualGovernanceConfigProvider configProvider;
    }

    ITimelock public immutable TIMELOCK;
    IResealManager public immutable RESEAL_MANAGER;
    address public immutable ESCROW_MASTER_COPY;

    // ---
    // Aspects
    // ---

    Proposers.Context internal _proposers;
    Tiebreaker.Context internal _tiebreaker;
    DualGovernanceStateMachine.Context internal _stateMachine;

    // ---
    // Standalone State Variables
    // ---
    IDualGovernanceConfigProvider internal _configProvider;
    address internal _resealCommittee;

    constructor(ExternalDependencies memory dependencies, SanityCheckParams memory sanityCheckParams) {
        TIMELOCK = dependencies.timelock;
        RESEAL_MANAGER = dependencies.resealManager;

        MIN_TIEBREAKER_ACTIVATION_TIMEOUT = sanityCheckParams.minTiebreakerActivationTimeout;
        MAX_TIEBREAKER_ACTIVATION_TIMEOUT = sanityCheckParams.maxTiebreakerActivationTimeout;
        MAX_SEALABLE_WITHDRAWAL_BLOCKERS_COUNT = sanityCheckParams.maxSealableWithdrawalBlockersCount;

        _setConfigProvider(dependencies.configProvider);

        ESCROW_MASTER_COPY = address(
            new Escrow({
                dualGovernance: this,
                stETH: dependencies.stETH,
                wstETH: dependencies.wstETH,
                withdrawalQueue: dependencies.withdrawalQueue,
                minWithdrawalsBatchSize: sanityCheckParams.minWithdrawalsBatchSize
            })
        );
        emit EscrowMasterCopyDeployed(ESCROW_MASTER_COPY);

        _stateMachine.initialize(dependencies.configProvider.getDualGovernanceConfig(), ESCROW_MASTER_COPY);
    }

    // ---
    // Proposals Flow
    // ---

    function submitProposal(ExternalCall[] calldata calls) external returns (uint256 proposalId) {
        _stateMachine.activateNextState(_configProvider.getDualGovernanceConfig(), ESCROW_MASTER_COPY);
        if (!_stateMachine.canSubmitProposal()) {
            revert ProposalSubmissionBlocked();
        }
        Proposers.Proposer memory proposer = _proposers.getProposer(msg.sender);
        proposalId = TIMELOCK.submit(proposer.executor, calls);
    }

    function scheduleProposal(uint256 proposalId) external {
        _stateMachine.activateNextState(_configProvider.getDualGovernanceConfig(), ESCROW_MASTER_COPY);
        ( /* id */ , /* status */, /* executor */, Timestamp submittedAt, /* scheduledAt */ ) =
            TIMELOCK.getProposalInfo(proposalId);
        if (!_stateMachine.canScheduleProposal(submittedAt)) {
            revert ProposalSchedulingBlocked(proposalId);
        }
        TIMELOCK.schedule(proposalId);
    }

    function cancelAllPendingProposals() external {
        _stateMachine.activateNextState(_configProvider.getDualGovernanceConfig(), ESCROW_MASTER_COPY);

        Proposers.Proposer memory proposer = _proposers.getProposer(msg.sender);
        if (proposer.executor != TIMELOCK.getAdminExecutor()) {
            revert NotAdminProposer();
        }

        State currentState = _stateMachine.getCurrentState();
        if (currentState != State.VetoSignalling && currentState != State.VetoSignallingDeactivation) {
            /// @dev Some proposer contracts, like Aragon Voting, may not support canceling decisions that have already
            /// reached consensus. This could lead to a situation where a proposer’s cancelAllPendingProposals() call
            /// becomes unexecutable if the Dual Governance state changes. However, it might become executable again if
            /// the system state shifts back to VetoSignalling or VetoSignallingDeactivation.
            /// To avoid such a scenario, an early return is used instead of a revert when proposals cannot be canceled
            /// due to an unsuitable Dual Governance state.
            emit CancelAllPendingProposalsSkipped();
            return;
        }

        TIMELOCK.cancelAllNonExecutedProposals();
        emit CancelAllPendingProposalsExecuted();
    }

    function canSubmitProposal() public view returns (bool) {
        return _stateMachine.canSubmitProposal();
    }

    function canScheduleProposal(uint256 proposalId) external view returns (bool) {
        ( /* id */ , /* status */, /* executor */, Timestamp submittedAt, /* scheduledAt */ ) =
            TIMELOCK.getProposalInfo(proposalId);
        return _stateMachine.canScheduleProposal(submittedAt) && TIMELOCK.canSchedule(proposalId);
    }

    // ---
    // Dual Governance State
    // ---

    function activateNextState() external {
        _stateMachine.activateNextState(_configProvider.getDualGovernanceConfig(), ESCROW_MASTER_COPY);
    }

    function setConfigProvider(IDualGovernanceConfigProvider newConfigProvider) external {
        _checkCallerIsAdminExecutor();
        _setConfigProvider(newConfigProvider);

        /// @dev the minAssetsLockDuration is kept as a storage variable in the signalling Escrow instance
        /// to sync the new value with current signalling escrow, it's value must be manually updated
        _stateMachine.signallingEscrow.setMinAssetsLockDuration(
            newConfigProvider.getDualGovernanceConfig().minAssetsLockDuration
        );
    }

    function getConfigProvider() external view returns (IDualGovernanceConfigProvider) {
        return _configProvider;
    }

    function getVetoSignallingEscrow() external view returns (address) {
        return address(_stateMachine.signallingEscrow);
    }

    function getRageQuitEscrow() external view returns (address) {
        return address(_stateMachine.rageQuitEscrow);
    }

    function getCurrentState() external view returns (State currentState) {
        currentState = _stateMachine.getCurrentState();
    }

    function getCurrentStateContext() external view returns (DualGovernanceStateMachine.Context memory) {
        return _stateMachine.getCurrentContext();
    }

    function getDynamicDelayDuration() external view returns (Duration) {
        return _stateMachine.getDynamicDelayDuration(_configProvider.getDualGovernanceConfig());
    }

    // ---
    // Proposers & Executors Management
    // ---

    function registerProposer(address proposer, address executor) external {
        _checkCallerIsAdminExecutor();
        _proposers.register(proposer, executor);
    }

    function unregisterProposer(address proposer) external {
        _checkCallerIsAdminExecutor();
        _proposers.unregister(proposer);

        /// @dev after the removal of the proposer, check that admin executor still belongs to some proposer
        if (!_proposers.isExecutor(TIMELOCK.getAdminExecutor())) {
            revert UnownedAdminExecutor();
        }
    }

    function isProposer(address account) external view returns (bool) {
        return _proposers.isProposer(account);
    }

    function getProposer(address account) external view returns (Proposers.Proposer memory proposer) {
        proposer = _proposers.getProposer(account);
    }

    function getProposers() external view returns (Proposers.Proposer[] memory proposers) {
        proposers = _proposers.getAllProposers();
    }

    function isExecutor(address account) external view returns (bool) {
        return _proposers.isExecutor(account);
    }

    // ---
    // Tiebreaker Protection
    // ---

    function addTiebreakerSealableWithdrawalBlocker(address sealableWithdrawalBlocker) external {
        _checkCallerIsAdminExecutor();
        _tiebreaker.addSealableWithdrawalBlocker(sealableWithdrawalBlocker, MAX_SEALABLE_WITHDRAWAL_BLOCKERS_COUNT);
    }

    function removeTiebreakerSealableWithdrawalBlocker(address sealableWithdrawalBlocker) external {
        _checkCallerIsAdminExecutor();
        _tiebreaker.removeSealableWithdrawalBlocker(sealableWithdrawalBlocker);
    }

    function setTiebreakerCommittee(address tiebreakerCommittee) external {
        _checkCallerIsAdminExecutor();
        _tiebreaker.setTiebreakerCommittee(tiebreakerCommittee);
    }

    function setTiebreakerActivationTimeout(Duration tiebreakerActivationTimeout) external {
        _checkCallerIsAdminExecutor();
        _tiebreaker.setTiebreakerActivationTimeout(
            MIN_TIEBREAKER_ACTIVATION_TIMEOUT, tiebreakerActivationTimeout, MAX_TIEBREAKER_ACTIVATION_TIMEOUT
        );
    }

    function tiebreakerResumeSealable(address sealable) external {
        _tiebreaker.checkCallerIsTiebreakerCommittee();
        _tiebreaker.checkTie(_stateMachine.getCurrentState(), _stateMachine.getNormalOrVetoCooldownStateExitedAt());
        RESEAL_MANAGER.resume(sealable);
    }

    function tiebreakerScheduleProposal(uint256 proposalId) external {
        _tiebreaker.checkCallerIsTiebreakerCommittee();
        _stateMachine.activateNextState(_configProvider.getDualGovernanceConfig(), ESCROW_MASTER_COPY);
        _tiebreaker.checkTie(_stateMachine.getCurrentState(), _stateMachine.getNormalOrVetoCooldownStateExitedAt());
        TIMELOCK.schedule(proposalId);
    }

    struct TiebreakerState {
        address tiebreakerCommittee;
        Duration tiebreakerActivationTimeout;
        address[] sealableWithdrawalBlockers;
    }

    function getTiebreakerState() external view returns (TiebreakerState memory tiebreakerState) {
        (
            tiebreakerState.tiebreakerCommittee,
            tiebreakerState.tiebreakerActivationTimeout,
            tiebreakerState.sealableWithdrawalBlockers
        ) = _tiebreaker.getTiebreakerInfo();
    }

    // ---
    // Reseal executor
    // ---

    function resealSealable(address sealable) external {
        if (msg.sender != _resealCommittee) {
            revert CallerIsNotResealCommittee(msg.sender);
        }
        if (_stateMachine.getCurrentState() == State.Normal) {
            revert ResealIsNotAllowedInNormalState();
        }
        RESEAL_MANAGER.reseal(sealable);
    }

    function setResealCommittee(address resealCommittee) external {
        _checkCallerIsAdminExecutor();
        _resealCommittee = resealCommittee;
    }

    // ---
    // Private methods
    // ---

    function _setConfigProvider(IDualGovernanceConfigProvider newConfigProvider) internal {
        if (address(newConfigProvider) == address(0)) {
            revert InvalidConfigProvider(newConfigProvider);
        }

        if (newConfigProvider == _configProvider) {
            return;
        }

        _configProvider = IDualGovernanceConfigProvider(newConfigProvider);
        emit ConfigProviderSet(newConfigProvider);
    }

    function _checkCallerIsAdminExecutor() internal view {
        if (TIMELOCK.getAdminExecutor() != msg.sender) {
            revert CallerIsNotAdminExecutor(msg.sender);
        }
    }
}

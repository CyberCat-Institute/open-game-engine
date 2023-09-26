pragma solidity >=0.8.2 <0.9.0;

/**
 * @title Piggybank
 * @dev Deposit and withdraw eth
 */
contract Piggybank {

    uint256 _balance;

    /**
     * @dev Deposit ether
     */
    function deposit() public payable {
        _balance = _balance + msg.value;
    }

    /**
     * @dev Withdraw a fixed amount of ether
     * @param _amount The amount to withdraw
    */
    function retrieve(uint256 _amount) public  {
        require(_balance >= _amount, "insufficient funds.");
        _balance = _balance - _amount;
        (bool res, ) = msg.sender.call{value: _amount}("");
        require(res, "transfer failed!");
    }
}


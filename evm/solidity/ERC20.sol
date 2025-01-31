// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity ^0.7.5;

import {DSMath} from "ds-math/math.sol";

contract ERC20 is DSMath {
    string  public constant name = "Token";
    string  public constant symbol = "TKN";
    uint8   public decimals = 18;
    uint256 public totalSupply;

    mapping (address => uint)                      public balanceOf;
    mapping (address => mapping (address => uint)) public allowance;

    event Approval(address indexed src, address indexed guy, uint amt);
    event Transfer(address indexed src, address indexed dst, uint amt);

    function transfer(address dst, uint amt) public returns (bool) {
        return transferFrom(msg.sender, dst, amt);
    }
    function transferFrom(address src, address dst, uint amt) public returns (bool) {
        require(balanceOf[src] >= amt, "insufficient-balance");

        if (src != msg.sender && allowance[src][msg.sender] != uint(-1)) {
            require(allowance[src][msg.sender] >= amt, "insufficient-allowance");
            allowance[src][msg.sender] = sub(allowance[src][msg.sender], amt);
            emit Approval(src, msg.sender, allowance[src][msg.sender]);
        }

        balanceOf[src] = sub(balanceOf[src], amt);
        balanceOf[dst] = add(balanceOf[dst], amt);
        emit Transfer(src, dst, amt);
        return true;
    }
    function approve(address usr, uint amt) public returns (bool) {
        allowance[msg.sender][usr] = amt;
        emit Approval(msg.sender, usr, amt);
        return true;
    }
}

contract MintableERC20 is ERC20 {
    // --- Auth ---
    address public owner;
    modifier auth() { require(msg.sender == owner, "unauthorized"); _; }

    // --- Init ---
    constructor() {
        owner = msg.sender;
    }

    // --- Mint/Burn ---
    function mint(address usr, uint amt) public auth {
        balanceOf[usr] = add(balanceOf[usr], amt);
        totalSupply    = add(totalSupply, amt);
        emit Transfer(address(0), usr, amt);
    }
    function burn(address usr, uint amt) public auth {
        balanceOf[usr] = sub(balanceOf[usr], amt);
        totalSupply    = sub(totalSupply, amt);
        emit Transfer(usr, address(0), amt);
    }
}


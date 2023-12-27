// SPDX-License-Identifier: AGPL-3.0-only
pragma solidity ^0.7.5;

import {ERC20} from "./ERC20.sol";

contract AMM is ERC20 {
    ERC20 token0;
    ERC20 token1;

    constructor(address _token0, address _token1) {
        token0 = ERC20(_token0);
        token1 = ERC20(_token1);
    }

    // join allows the caller to exchange amt0 and amt1 tokens for some amount
    // of pool shares. The exact amount of pool shares minted depends on the
    // state of the pool at the time of the call.
    function join(uint amt0, uint amt1) external {
        require(amt0 > 0 && amt1 > 0, "insufficient input amounts");

        uint bal0 = token0.balanceOf(address(this));
        uint bal1 = token1.balanceOf(address(this));

        uint shares = totalSupply == 0
                      ? min(amt0, amt1)
                      : min(mul(totalSupply, amt0) / bal0,
                            mul(totalSupply, amt1) / bal1);

        balanceOf[msg.sender] = add(balanceOf[msg.sender], shares);
        totalSupply = add(totalSupply, shares);

        token0.transferFrom(msg.sender, address(this), amt0);
        token1.transferFrom(msg.sender, address(this), amt1);
    }

    // exit allows the caller to exchange shares pool shares for the
    // proportional amount of the underlying tokens.
    function exit(uint shares) external {
        uint amt0 = mul(token0.balanceOf(address(this)), shares) / totalSupply;
        uint amt1 = mul(token1.balanceOf(address(this)), shares) / totalSupply;

        balanceOf[msg.sender] = sub(balanceOf[msg.sender], shares);
        totalSupply = sub(totalSupply, shares);

        token0.transfer(msg.sender, amt0);
        token1.transfer(msg.sender, amt1);
    }

    // swap allows the caller to exchange amt of src for dst at a price given
    // by the constant product formula: x * y == k.
    function swap(address src, address dst, uint amt) external {
        require(src != dst, "no self swap");
        require(src == address(token0) || src == address(token1), "src not in pair");
        require(dst == address(token0) || dst == address(token1), "dst not in pair");

        uint K = mul(token0.balanceOf(address(this)), token1.balanceOf(address(this)));

        ERC20(src).transferFrom(msg.sender, address(this), amt);

        uint out = sub(
            ERC20(dst).balanceOf(address(this)),
            K / ERC20(src).balanceOf(address(this)) + 1 // rounding
        );

        ERC20(dst).transfer(msg.sender, out);

        uint KPost = mul(token0.balanceOf(address(this)), token1.balanceOf(address(this)));
        assert(KPost >= K);
    }

}

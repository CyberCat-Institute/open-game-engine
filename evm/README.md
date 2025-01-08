## Open-games HEVM integration

This project integrates open games with HEVM, an execution environement for EVM bytecode.

This allows the game-theoretic analysis of smart contracts

## How to build lido contracts

- run `nix develop .` in the parent directory, this will take care of GHC, Stack and solidity.
- copy the content of the contracts/ directory from the lido project (https://github.com/lidofinance/dual-governance/tree/main/contracts) into here (the evm/ directory of the open games project).
- download the openzepplin dependency with `npm install openzeppelin`.
- copy the contracts from openzeppelin into a `@openzeppelin` folder in here. The path should be `evm/@openzeppelin`. The following command should work `cp -r node_modules/openzeppelin/ @openzeppelin`.
- build the project with `stack build`, you can inspect the imported functions with `stack repl` and using `:browse` after importing the lido module from the examples.

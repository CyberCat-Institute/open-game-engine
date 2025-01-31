
# Open games architecture around Act contracts

An act contract is a program that represents some contract on the Ethereum blockchain.

They are used to find vulnerabilities in contract implementations.

Open games are a compositional tool for game theory. The tool we developped
translates act contracts into open games to allow for game-theoretic analysis.

Act finds implementation bugs. Open games find economical bugs.

## Act contracts in open games

- we translate each contract into a single function
- we combine each contracts together into a "blochain execution" function
- we lift our blockchain execution into an open game
- You can now submit transactions to it and see the result. Analysing for
equilibrium.

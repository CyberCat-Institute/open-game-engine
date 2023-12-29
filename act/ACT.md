# Act integration with Open games

Act integration with open games can be used with the `act` target of the project.

In its current state it will run `act/Main.hs` which will import `Examples.AmmGenerated`
and `Examples.player`. The generated version is automatically derived from an ACT
file, the `Player` uses the generated code to setup a game with transactions and players
sending those transactions.

## How to use

We split our example into 3 files:

- `Examples.AmmGenerated` for template-haskell code
- `Examples.Player` for strategic analysis
- `Main` for running the analysis

### `AmmGenerated`

`Examples.AmmGenerated` shows an example of how to import an ACT program into an
open game for analysis.

```haskell
import Act.TH
import Act.Prelude
import Act.Execution

-- This generates the `ammContract`
$(act2OG "amm.act")

-- This combines two contracts with non-shared state
twoAmms = combine (unionContracts ("amm1", ammContract) ("amm2", ammContract))
```

The first three imports are essential to import the `act2OG` splice which will generate
all the top-level declarations necessary to interact with the contract. In particular it
will generate a definition for a _type_ that represents the state of the contract. In
our amm example the type is called "AmmState". The name will always be the name of the
contract followed by "State".
We also generate a top-level function that takes a `Transaction` and a _State_ and
returns a new state. This function re-creates the operations defined in the original ACT
specification. It can then be re-used within an open game as a non-strategic player since
it is now a plain haskell function. Finally the `combine` function comes from `Act.Execution`
which provides some utilities to interact with contract-functions generated.

### `Player`

Our contract-function is then instanciated as an open game where the state of the game is
threaded to the function. You can see this in the function `playerAutomatic` which takes
two amm states, and runs a series of transactions on both

### `Main`

The main file is useful in order to start playing around with this setup, it is not necessary
to use it at all. It is enough to import `ACT.TH`, `ACT.Prelude` and possibly `ACT.Execution`
in order to start using the ACT translation layer.

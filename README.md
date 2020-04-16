# Open games in Haskell

This is a Haskell combinator library implementing open games, a battery of examples, and a code generation tool for making the combinator library practical to work with.

This tool allows modular definition of games, and then checking (but not computing) different types of Nash equilibria. After entering a non-equilibrium, the tool can generate detailed information about deviations.

For background on open games, see these two papers:
* [Compositional game theory](https://arxiv.org/abs/1603.04641) by Ghani, Hedges, Winschel and Zahn
* [Bayesian open games](https://arxiv.org/abs/1910.03656) by Bolt, Hedges and Zahn

I hope that this tool will be usable without in-depth knowledge of how it works or how open games work, but I can't make any promises.

From a user's perspective, the examples in `OpenGames.Examples` are intended to be self-documenting. Studying the modules in that directory will be more effective than any documentation I could write.

When I have time I plan to write a paper describing how the preprocessor works.

Feel free to [contact me](mailto:juleshedges.invariant@gmail.com) (ie. [Jules Hedges](https://julesh.com/)) if you have specific questions. ("Why doesn't this block compile correctly?" is a reasonable question.)

Also contact me if you'd like to contribute! A concrete syntax and parser for blocks is an example of something that would be easy to add for a better programmer than me.

Other contributions not recorded by GitHub (because this is a copy of a private repository): Philipp Zahn, Sjoerd Visscher

## The preprocessor

In order to use the preprocessor, create a value of type `OpenGames.Preprocessor.AbstractSyntax.Block`, and then interactively (in GHCi) apply the function `OpenGames.Preprocessor.Preprocessor.compileBlock`. The resulting `Show` instance will result in a string containing Haskell code. Copy this code from the terminal into a file that makes the appropriate imports from `OpenGames.Engine`.

Examples of blocks can be seen in `OpenGames.Examples.Source`, and in each case the resulting generated code can be seen in the file of the same name in `OpenGames.Examples`.

The scoping rules for blocks are quite complicated, and reflect the topological rules for string diagrams of open games:
* Block inputs and line outputs (both covariant and contravariant) are variables, which are brought into scope (I think they could be general patterns, but I haven't tested it properly)
* Covariant inputs of lines are expressions which can contain variables brought into scope by the covariant inputs of the block, and the covariant outputs of any previous line
* The covariant outputs of the block are expressions which can contain variables brought into scope by the covariant inputs of the block and the covariant outputs of any line
* Contravariant inputs of lines are expressions which can contain variables brought into scope by any inputs of the block, the covariant outputs of any line, and the contravariant outputs of any later line
* The contravariant outputs of the block are expressions which can contain variables brought into scope by any inputs of the block and any outputs of any line

(Think: covariant values flow downwards, then turn around and come back upwards. Contravariant values only flow upwards.)

The preprocessor does no scope or type checking, so if you make a mistake then you will get a (probably large) error message when you compile the generated code.

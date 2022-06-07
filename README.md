# Getting started with open games

This tutorial is about how to install the tools to use open games and how to master the language
of open games to write powerful games and analyze them.

This repo is a refactored and simplified implementation on the basis of [this](https://github.com/jules-hedges/open-game-engine) version by Jules Hedges.

If you have questions, drop me (Philipp) a [mail](mailto:philipp.zahn@unisg.ch)!

This repo is work in progress. Expect changes at any time!

# What are open games?

_Open games_ are a mathematical structure allowing you to describe game-theoretical games. _Open-games-hs_
is a framework to write those games in a programmatic way and analyze those games. The framework is
written in Haskell and this allows Open Games to inherit a lot of features from the haskell ecosystem such
as datatypes, functions and the large set of haskell libraries.

_Open-games-hs_ is a framework implementing the theory of _Open games_ with which you can write a program that
describes a game and its players. You can supply strategies for the game and test the game for equilibrium.
If the game does not reach equilibrium, the list of deviations
is printed and the reason why the player want to deviate is recorded. The biggest strength of open games
is the ability to build your game from smaller modular components that you can
swap out or parameterize.

# Modelling in open games

This [tutorial](https://github.com/philipp-zahn/open-games-hs/blob/master/Tutorial/TUTORIAL.md) shows how to use the software for modelling.


# How to install and run open-games-hs

Open-games-hs requires stack and a text editor, for the text editor, it is very likely
that your existing one already supports haskell. If you do not have one I recommend starting with [VSCode][VSCODE].

You can install stack following the instructions here: https://docs.haskellstack.org/en/stable/install_and_upgrade/

Stack will be responsible for installing haskell, the Open-games-hs framework and its dependencies.

Once stack is installed you can run the demo project by running `stack run`. That will execute the project, and
print the result of executing an equilibrium check on a very simple game. The rest of the tutorial will go into how
to use the open-games framework in order to design and analyse games interactively using `ghci`. To invoke it, use
`stack ghci` and that will start a new interactive session.

# Designing and analyzing games interactively

During an interactive session you can:

- execute programs
- recompile the project with `:r`
- obtain documentation about a function with `:i`
- query the type of an expression with `:t`

Most of the programs you will execute will print the result of analyzing a game. In the demo project, the main function
perform an analysis of two simple games, the first one is in equilibrium and the second one exhibits profitable deviations. To
run the program from the interactive sessions type `main`.

# Graph dependency visualiser

There is a rudimentary dependency visualizer for debugging (and inspecting larger games).

If you run `stack run graphics`, a `dotfile` is created. This is a graphviz file that can be interpreted with graphviz with the following command:

    dot -Tsvg dotfile > output.svg

This will create an SVG that you can open with any SVG viewer (like a web browser). The graph is generated from the `parseTree` of a game. Files of this form need to be located in `graphics/Main.hs` where the main function simply prints the dot file from the game passed in argument. If you want to use a different game, you can pass it a new parsetree using the `parseTree` quasiquote.

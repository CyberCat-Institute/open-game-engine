# Overview

This is a Haskell combinator library implementing _open games_, a battery of examples, and a code generation tool for making the combinator library practical to work with.

This repo is a refactored and simplified implementation on the basis of [this](https://github.com/jules-hedges/open-game-engine) version by Jules Hedges.

If you have questions, drop me (Philipp) a [mail](mailto:philipp.zahn@unisg.ch)!

This repo is work in progress. Expect changes at any time!

# Installation

You can use `stack build` to compile the project, `stack test` will run the tests
`stack ghci` and `stack ghci --test` will run ghci using the main target or the test
targets respectively.


# Modelling in the software framework

This [tutorial](https://github.com/philipp-zahn/open-games-hs/blob/master/Tutorial/TUTORIAL.md) shows how to use the software for modelling.


# Graph dependency visualiser

There is a rudimentary dependency visualizer for debugging (and inspecting larger games).

If you run `stack run graphics`, a `dotfile` is created. This is a graphviz file that can be interpreted with graphviz with the following command:

    dot -Tsvg dotfile > output.svg

This will create an SVG that you can open with any SVG viewer (like a web browser). The graph is generated from the `parseTree` of a game. Files of this form need to be located in `graphics/Main.hs` where the main function simply prints the dot file from the game passed in argument. If you want to use a different game, you can pass it a new parsetree using the `parseTree` quasiquote.

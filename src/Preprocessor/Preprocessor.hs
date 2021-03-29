module Preprocessor.Preprocessor where

-- This file is where the magic happens

import Data.List (inits, tails)

import Preprocessor.AbstractSyntax
import Preprocessor.Types

sequentialTrivialL, sequentialTrivialR, simultaneousTrivialL, simultaneousTrivialR :: FreeOpenGame String String -> FreeOpenGame String String -> FreeOpenGame String String
sequentialTrivialL g h   = Reindex UnitIntroL (Sequential g h) -- \x -> ((), x)
sequentialTrivialR g h   = Reindex UnitIntroR (Sequential g h) -- \x -> (x, ())
simultaneousTrivialL g h = Reindex UnitIntroL (Simultaneous g h) -- \x -> ((), x)
simultaneousTrivialR g h = Reindex UnitIntroR (Simultaneous g h) -- \x -> (x, ())
-- Pre-compilation pass to figure out what's in scope at each point

data LineWithContext = LineWithContext {
  line :: Line String String,
  covariantContext :: Variables String,
  contravariantContext :: Variables String}

covariantContexts :: Block String String -> [Variables String]
covariantContexts block = map f (init (inits (map (Variables . covariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (Variables (blockCovariantInputs block) : contexts)

contravariantContexts :: Block String String -> [Variables String]
contravariantContexts block = map (f . reverse) (tail (tails (map (Variables . contravariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (concat [[Variables (blockCovariantInputs block)],
                                                   map (Variables . covariantOutputs) (blockLines block),
                                                   [Variables (blockContravariantInputs block)],
                                                   contexts])

linesWithContext :: Block String String -> [LineWithContext]
linesWithContext block = zipWith3 LineWithContext (blockLines block) (covariantContexts block) (contravariantContexts block)

-- The business end of the compiler

compileLine :: LineWithContext -> FreeOpenGame String String
compileLine (LineWithContext l cov con) = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where l1 = Function (CopyLambda cov (Expressions (covariantInputs l))) (Multiplex con (Variables (contravariantOutputs l)))
        l2 = Function Identity Identity `simultaneousTrivialL` Atom ((matrix l))
        l3 = Function (Multiplex cov (Variables (covariantOutputs l))) (CopyLambda con (Expressions (contravariantInputs l)))

compileBlock :: Block String String -> FreeOpenGame String String
compileBlock block = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where lines = linesWithContext block
        covariantBlockContext = flattenVariables [covariantContext (last lines), Variables (covariantOutputs (line (last lines)))]
        contravariantBlockContext = flattenVariables [contravariantContext (head lines), Variables (contravariantOutputs (line (head lines)))]
        l1 = Function Identity (Lambda contravariantBlockContext (Expressions (blockContravariantOutputs block)))
        l2 = Reindex (FlattenTuples (length lines)) (foldl1 Sequential (map compileLine lines))
        l3 = Lens (Lambda covariantBlockContext (Expressions (blockCovariantOutputs block)))
                  (Curry (Multiplex covariantBlockContext (Variables (blockContravariantInputs block))))

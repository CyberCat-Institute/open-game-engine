module OpenGames.Preprocessor.Preprocessor where

-- This file is where the magic happens

import Data.List (inits, tails)

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.FreeOpenGames

-- Pre-compilation pass to figure out what's in scope at each point

data LineWithContext = LineWithContext {
  line :: Line,
  covariantContext :: VariableList,
  contravariantContext :: VariableList}

covariantContexts :: Block -> [VariableList]
covariantContexts block = map f (init (inits (map (VariableList . covariantOutputs) (blockLines block))))
  where f contexts = flattenVariableList (VariableList (blockCovariantInputs block) : contexts)

contravariantContexts :: Block -> [VariableList]
contravariantContexts block = map (f . reverse) (tail (tails (map (VariableList . contravariantOutputs) (blockLines block))))
  where f contexts = flattenVariableList (concat [[VariableList (blockCovariantInputs block)],
                                                   map (VariableList . covariantOutputs) (blockLines block),
                                                   [VariableList (blockContravariantInputs block)],
                                                   contexts])

linesWithContext :: Block -> [LineWithContext]
linesWithContext block = zipWith3 LineWithContext (blockLines block) (covariantContexts block) (contravariantContexts block)

-- The business end of the compiler

compileLine :: LineWithContext -> FreeOpenGame
compileLine (LineWithContext l cov con) = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where l1 = Function (CopyLambda cov (ExpressionList (covariantInputs l))) (Multiplex con (VariableList (contravariantOutputs l)))
        l2 = Function Identity Identity `simultaneousTrivialL` Atom (AtomExpression (matrix l))
        l3 = Function (Multiplex cov (VariableList (covariantOutputs l))) (CopyLambda con (ExpressionList (contravariantInputs l)))

compileBlock :: Block -> FreeOpenGame
compileBlock block = l1 `sequentialTrivialL` (l2 `sequentialTrivialR` l3)
  where lines = linesWithContext block
        covariantBlockContext = flattenVariableList [covariantContext (last lines), VariableList (covariantOutputs (line (last lines)))]
        contravariantBlockContext = flattenVariableList [contravariantContext (head lines), VariableList (contravariantOutputs (line (head lines)))]
        l1 = Function Identity (Lambda contravariantBlockContext (ExpressionList (blockContravariantOutputs block)))
        l2 = Reindex (FlattenTuples (length lines)) (foldl1 Sequential (map compileLine lines))
        l3 = Lens (Lambda covariantBlockContext (ExpressionList (blockCovariantOutputs block)))
                  (Curry (Multiplex covariantBlockContext (VariableList (blockContravariantInputs block))))

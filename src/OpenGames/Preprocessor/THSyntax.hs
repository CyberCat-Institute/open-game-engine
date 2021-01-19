{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module OpenGames.Preprocessor.THSyntax ( GBlock(..)
                                       , LineWithContext(..)
                                       , compileBlock
                                       , param
                                       , compileLine
                                       , generateGame
                                       , block
                                       , line
                                       )
                                       where

import Language.Haskell.TH.Syntax
import OpenGames.Preprocessor.TH
import Data.List (inits, tails)

data GBlock l e = GBlock {
  blockCovariantInputs :: [String], blockContravariantOutputs :: [e],
  blockLines :: [l],
  blockCovariantOutputs :: [e], blockContravariantInputs :: [String]}


type SBlock = GBlock SLine Exp
type QBlock = GBlock QLine (Q Exp)

block :: [String] -> [Q Exp] -> [QLine] -> [Q Exp] -> [String] -> QBlock
block = GBlock


data GLine e = GLine {
  covariantInputs :: [e], contravariantOutputs :: [String],
  matrix :: e, --
  covariantOutputs :: [String], contravariantInputs :: [e]}

type SLine = GLine Exp
type QLine = GLine (Q Exp)

line :: [Q Exp] -> [String] -> Q Exp -> [String] -> [Q Exp] -> QLine
line = GLine

data LineWithContext = LineWithContext {
  lineContext :: SLine,
  covariantContext :: Variables,
  contravariantContext :: Variables}

class ToLine line where
  toLine :: line -> Q SLine

instance ToLine SLine where
  toLine = pure

instance ToLine QLine where
  toLine = compileQLine

class ToExpr blockExpr where
  toExpr :: blockExpr -> Q Exp

instance ToExpr String where
  toExpr = pure . VarE . mkName

instance ToExpr Exp where
  toExpr = pure

instance ToExpr (Q Exp) where
  toExpr = id

-- The business end of the compiler

compileLine :: LineWithContext -> FreeOpenGame
compileLine (LineWithContext l cov con) = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where l1 = Function (CopyLambda cov (Expressions (covariantInputs l))) (Multiplex con (Variables (contravariantOutputs l)))
        l2 = Function Identity Identity `simultaneousTrivialL` Atom (matrix l)
        l3 = Function (Multiplex cov (Variables $ (covariantOutputs l))) (CopyLambda con (Expressions (contravariantInputs l)))

compileBlock :: SBlock -> FreeOpenGame
compileBlock block = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where lines :: [LineWithContext]
        lines = linesWithContext block
        covariantBlockContext = flattenVariables [
          covariantContext (last lines) , Variables (covariantOutputs (lineContext (last lines)))]
        contravariantBlockContext = flattenVariables [contravariantContext (head lines)
                                                     , Variables (contravariantOutputs (lineContext (head lines)))]
        l1 = Function Identity (Lambda contravariantBlockContext (Expressions (blockContravariantOutputs block)))
        l2 = Reindex (FlattenTuples (length lines)) (foldl1 Sequential (map compileLine lines))
        l3 = Lens (Lambda covariantBlockContext (Expressions (blockCovariantOutputs block)))
                  (Curry (Multiplex covariantBlockContext (Variables (blockContravariantInputs block))))

newtype AtomExpression = AtomExpression Exp

flattenVariables = Variables . concat . map vars

sequentialTrivialL, sequentialTrivialR, simultaneousTrivialL, simultaneousTrivialR :: FreeOpenGame -> FreeOpenGame -> FreeOpenGame
sequentialTrivialL g h   = Reindex UnitIntroL (Sequential g h)
sequentialTrivialR g h   = Reindex UnitIntroR (Sequential g h)
simultaneousTrivialL g h = Reindex UnitIntroL (Simultaneous g h)
simultaneousTrivialR g h = Reindex UnitIntroR (Simultaneous g h)

covariantContexts :: SBlock -> [Variables]
covariantContexts block = map f (init (inits (map (Variables . covariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (Variables (blockCovariantInputs block) : contexts)


contravariantContexts :: SBlock -> [Variables]
contravariantContexts block = map (f . reverse) (tail (tails (map (Variables . contravariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (concat [[Variables (blockCovariantInputs block)],
                                                   map (Variables . covariantOutputs) (blockLines block),
                                                   [Variables (blockContravariantInputs block)],
                                                   contexts])

linesWithContext :: SBlock -> [LineWithContext]
linesWithContext block = zipWith3 LineWithContext (blockLines block) (covariantContexts block) (contravariantContexts block)

param :: String -> Q Exp
param = pure . VarE . mkName

compileQLine :: QLine -> Q SLine
compileQLine qline = do covIn <- traverse id $ covariantInputs qline
                        conIn <- traverse id $ contravariantInputs qline
                        exp <- matrix qline
                        let covOut = covariantOutputs qline
                        let conOut = contravariantOutputs qline
                        pure $ GLine covIn conOut exp covOut conIn


toBlock :: (ToLine l, ToExpr e) => GBlock l e -> Q SBlock
toBlock (GBlock a b lines c d) = do l' <- traverse toLine lines
                                    b' <- traverse toExpr b
                                    c' <- traverse toExpr c
                                    pure $ GBlock a b' l' c' d

class GameCompiler term where
  generateGame :: String -> [String] -> term -> Q [Dec]

instance (ToLine line, ToExpr e) => GameCompiler (GBlock line e) where
  generateGame name args block =
    do blk <- toBlock block
       game <- interpretOpenGame (compileBlock blk)
       pure $ [FunD (mkName name) [Clause (fmap (VarP . mkName) args) (NormalB game) []]]

instance GameCompiler ([QLine]) where
  generateGame name args lines = do lines <- traverse compileQLine lines
                                    generateGame name args $ (GBlock [] [] lines [] [] :: SBlock)


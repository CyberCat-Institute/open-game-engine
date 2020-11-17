{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module OpenGames.Preprocessor.THSyntax ( SLine(..)
                                       , QLine(..)
                                       , Block(..)
                                       , GBlock(..)
                                       , LineWithContext(..)
                                       , compileBlock
                                       , param
                                       , compileLine
                                       , generateGame
                                       )
                                       where

import Language.Haskell.TH.Syntax
import OpenGames.Preprocessor.TH
import Data.List (inits, tails)

data GBlock l = GBlock {
  blockCovariantInputs :: [String], blockContravariantOutputs :: [String],
  blockLines :: [l],
  blockCovariantOutputs :: [String], blockContravariantInputs :: [String]}

type Block = GBlock SLine

data SLine = SLine {
  covariantInputs :: [Exp], contravariantOutputs :: [String],
  matrix :: Exp, --
  covariantOutputs :: [String], contravariantInputs :: [Exp]}

data QLine = QLine {
  qcovariantInputs :: [Q Exp], qcontravariantOutputs :: [String],
  qmatrix :: Q Exp, --
  qcovariantOutputs :: [String], qcontravariantInputs :: [Q Exp]}

data LineWithContext = LineWithContext {
  line :: SLine,
  covariantContext :: Variables,
  contravariantContext :: Variables}

class ToLine line where
  toLine :: line -> Q SLine

instance ToLine SLine where
  toLine = pure

instance ToLine QLine where
  toLine = compileQLine

-- The business end of the compiler

compileLine :: LineWithContext -> FreeOpenGame
compileLine (LineWithContext l cov con) = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where l1 = Function (CopyLambda cov (Expressions (covariantInputs l))) (Multiplex con (Variables (contravariantOutputs l)))
        l2 = Function Identity Identity `simultaneousTrivialL` Atom (matrix l)
        l3 = Function (Multiplex cov (Variables $ (covariantOutputs l))) (CopyLambda con (Expressions (contravariantInputs l)))

compileBlock :: Block -> FreeOpenGame
compileBlock block = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where lines :: [LineWithContext]
        lines = linesWithContext block
        covariantBlockContext = flattenVariables [
          covariantContext (last lines) , Variables (covariantOutputs (line (last lines)))]
        contravariantBlockContext = flattenVariables [contravariantContext (head lines)
                                                     , Variables (contravariantOutputs (line (head lines)))]
        l1 = Function Identity (Lambda contravariantBlockContext (Expressions (map (VarE . mkName) $ blockContravariantOutputs block)))
        l2 = Reindex (FlattenTuples (length lines)) (foldl1 Sequential (map compileLine lines))
        l3 = Lens (Lambda covariantBlockContext (Expressions (map (VarE . mkName) $ blockCovariantOutputs block)))
                  (Curry (Multiplex covariantBlockContext (Variables (blockContravariantInputs block))))

newtype AtomExpression = AtomExpression Exp

flattenVariables = Variables . concat . map vars

sequentialTrivialL, sequentialTrivialR, simultaneousTrivialL, simultaneousTrivialR :: FreeOpenGame -> FreeOpenGame -> FreeOpenGame
sequentialTrivialL g h   = Reindex UnitIntroL (Sequential g h)
sequentialTrivialR g h   = Reindex UnitIntroR (Sequential g h)
simultaneousTrivialL g h = Reindex UnitIntroL (Simultaneous g h)
simultaneousTrivialR g h = Reindex UnitIntroR (Simultaneous g h)

covariantContexts :: Block -> [Variables]
covariantContexts block = map f (init (inits (map (Variables . covariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (Variables (blockCovariantInputs block) : contexts)


contravariantContexts :: Block -> [Variables]
contravariantContexts block = map (f . reverse) (tail (tails (map (Variables . contravariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (concat [[Variables (blockCovariantInputs block)],
                                                   map (Variables . covariantOutputs) (blockLines block),
                                                   [Variables (blockContravariantInputs block)],
                                                   contexts])

linesWithContext :: Block -> [LineWithContext]
linesWithContext block = zipWith3 LineWithContext (blockLines block) (covariantContexts block) (contravariantContexts block)

param :: String -> Q Exp
param = pure . VarE . mkName

compileQLine :: QLine -> Q SLine
compileQLine qline = do covIn <- traverse id $ qcovariantInputs qline
                        conIn <- traverse id $ qcontravariantInputs qline
                        exp <- qmatrix qline
                        let covOut = qcovariantOutputs qline
                        let conOut = qcontravariantOutputs qline
                        pure $ SLine covIn conOut exp covOut conIn


toBlock :: ToLine l => GBlock l -> Q Block
toBlock (GBlock a b lines c d) = do l' <- traverse toLine lines
                                    pure $ GBlock a b l' c d

class GameCompiler term where
  generateGame :: String -> [String] -> term -> Q [Dec]

instance (ToLine line) => GameCompiler (GBlock line) where
  generateGame name args block =
    do blk <- toBlock block
       game <- interpretOpenGame (compileBlock blk)
       pure $ [FunD (mkName name) [Clause (fmap (VarP . mkName) args) (NormalB game) []]]

instance GameCompiler ([QLine]) where
  generateGame name args lines = do lines <- traverse compileQLine lines
                                    generateGame name args $ GBlock [] [] lines [] []


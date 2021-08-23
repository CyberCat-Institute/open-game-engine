{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Preprocessor.THSyntax ( LineWithContext(..)
                                       , SLine
                                       , QLine
                                       , compileBlock
                                       , param
                                       , asPat
                                       , compileLine
                                       , generateGame
                                       )
                                       where

import Language.Haskell.TH.Syntax
import OpenGames.Preprocessor.TH
import OpenGames.Preprocessor.Types
import OpenGames.Preprocessor.AbstractSyntax
import Data.List (inits, tails)
import Data.Bifunctor


type SLine = Line (Maybe String) Pat Exp
type QLine = Line (Maybe String) String (Q Exp)
type GBlock = Block SLine

data LineWithContext p e = LineWithContext {
  line :: Line (Maybe String) p e,
  covariantContext :: Variables p,
  contravariantContext :: Variables p}

class ToLine pat exp where
  toLine :: Line (Maybe String) pat exp -> Q SLine

instance ToLine Pat Exp where
  toLine = pure

instance ToLine String (Q Exp) where
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

compileLine :: LineWithContext p e -> FreeOpenGame p e
compileLine (LineWithContext l cov con) = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where l1 = Function (CopyLambda cov (Expressions (covariantInputs l))) (Multiplex con (Variables (contravariantOutputs l)))
        l2 = Function Identity Identity `simultaneousTrivialL` Atom (matrix l)
        l3 = Function (Multiplex cov (Variables $ (covariantOutputs l))) (CopyLambda con (Expressions (contravariantInputs l)))

compileBlock :: forall p e. Block p e -> FreeOpenGame p e
compileBlock block = (l1 `sequentialTrivialL` l2) `sequentialTrivialR` l3
  where lines :: [LineWithContext p e]
        lines = linesWithContext block
        covariantBlockContext = flattenVariables [
          covariantContext (last lines) , Variables (covariantOutputs (line (last lines)))]
        contravariantBlockContext = flattenVariables [contravariantContext (head lines)
                                                     , Variables (contravariantOutputs (line (head lines)))]
        l1 = Function Identity (Lambda contravariantBlockContext (Expressions (blockContravariantOutputs block)))
        l2 = Reindex (FlattenTuples (length lines)) (foldl1 Sequential (map compileLine lines))
        l3 = Lens (Lambda covariantBlockContext (Expressions (blockCovariantOutputs block)))
                  (Curry (Multiplex covariantBlockContext (Variables (blockContravariantInputs block))))

sequentialTrivialL, sequentialTrivialR, simultaneousTrivialL, simultaneousTrivialR :: FreeOpenGame p e -> FreeOpenGame p e -> FreeOpenGame p e
sequentialTrivialL g h   = Reindex UnitIntroL (Sequential g h)
sequentialTrivialR g h   = Reindex UnitIntroR (Sequential g h)
simultaneousTrivialL g h = Reindex UnitIntroL (Simultaneous g h)
simultaneousTrivialR g h = Reindex UnitIntroR (Simultaneous g h)

covariantContexts :: Block p e -> [Variables p]
covariantContexts block = map f (init (inits (map (Variables . covariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (Variables (blockCovariantInputs block) : contexts)


contravariantContexts :: Block p e -> [Variables p]
contravariantContexts block = map (f . reverse) (tail (tails (map (Variables . contravariantOutputs) (blockLines block))))
  where f contexts = flattenVariables (concat [[Variables (blockCovariantInputs block)],
                                                   map (Variables . covariantOutputs) (blockLines block),
                                                   [Variables (blockContravariantInputs block)],
                                                   contexts])

linesWithContext :: Block p e -> [LineWithContext p e]
linesWithContext block = zipWith3 LineWithContext (blockLines block) (covariantContexts block) (contravariantContexts block)

param :: String -> Q Exp
param = pure . VarE . mkName

asPat :: String -> Q Pat
asPat = pure . VarP . mkName

compileQLine :: QLine -> Q SLine
compileQLine qline = do covIn <- traverse id $ covariantInputs qline
                        conIn <- traverse id $ contravariantInputs qline
                        exp <- matrix qline
                        let covOut = fmap (VarP . mkName) (covariantOutputs qline)
                        let conOut = fmap (VarP . mkName) (contravariantOutputs qline)
                        pure $ Line Nothing covIn conOut exp covOut conIn


class GameCompiler term where
  generateGame :: String -> [String] -> term -> Q [Dec]

instance GameCompiler (Block Pat Exp) where
  generateGame name args block =
    do
       game <- interpretOpenGame (compileBlock block)
       pure $ [FunD (mkName name) [Clause (fmap (VarP . mkName) args) (NormalB game) []]]

extract :: Block (Q p) (Q e) -> Q (Block p e)
extract (Block covIn conOut lines covOut conIn) =
  do covIn' <- sequence covIn
     conOut' <- sequence conOut
     lines' <- traverse extractLines lines
     covOut' <- sequence covOut
     conIn' <- sequence conIn
     pure (Block covIn' conOut' lines' covOut' conIn')
  where
    extractLines :: Line (Maybe String) (Q p) (Q e) -> Q (Line (Maybe String) p e)
    extractLines (Line lbl covIn conOut m covOut conIn) = do
      covIn' <- sequence covIn
      conOut' <- sequence conOut
      body <- m
      covOut' <- sequence covOut
      conIn' <- sequence conIn
      pure (Line lbl covIn' conOut' body covOut' conIn')

instance GameCompiler (Block (Q Pat) (Q Exp)) where
  generateGame name args block =
    extract block >>=
    generateGame name args

instance GameCompiler (Block String (Q Exp)) where
  generateGame name args block = do
    b <- sequence block
    generateGame name args (first (VarP . mkName) b)


instance GameCompiler ([QLine]) where
  generateGame name args lines = do lines <- traverse compileQLine lines
                                    generateGame name args $ Block [] [] lines [] []


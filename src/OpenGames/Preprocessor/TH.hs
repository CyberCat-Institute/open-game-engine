{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module OpenGames.Preprocessor.TH (Variables(..)
                                 , Expressions(..)
                                 , FreeOpenGame(..)
                                 , ReindexingExpression(..)
                                 , FunctionExpression(..)
                                 , interpretOpenGame
                                 , interpretFunction
                                 ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

newtype Variables   = Variables {vars :: [String]} deriving Show
newtype Expressions = Expressions {exps :: [Exp]}

data FunctionExpression = Identity                         -- \x -> x
                        | Copy                             -- \x -> (x, x)
                        | Lambda Variables Expressions     -- \(x1, ..., xm) -> (e1, ..., en)
                        | CopyLambda Variables Expressions -- \(x1, ..., xm) -> ((x1, ..., xm), (e1, ..., en))
                        | Multiplex Variables Variables    -- \((x1, ..., xm), (y1, ..., yn)) -> (x1, ..., xm, y1, ..., yn)
                        | Curry FunctionExpression         -- curry f

combinePats :: [Pat] -> Pat
combinePats [x] = x
combinePats xs = TupP xs

interpretFunction :: FunctionExpression -> Q Exp
interpretFunction Identity = [| id |]
interpretFunction Copy = [| \x -> (x, x) |]
interpretFunction (Lambda (Variables {vars}) (Expressions {exps})) =
  pure $ LamE (pure $ TupP $ map (VarP . mkName) vars) (TupE exps)
interpretFunction (CopyLambda (Variables { vars }) (Expressions { exps })) =
  pure $ LamE (pure $ TupP $ map (VarP . mkName) vars) (TupE [TupE $ map (VarE . mkName) vars, TupE exps])
interpretFunction (Multiplex (Variables { vars }) (Variables { vars = vars' })) =
  pure $ LamE (pure $ TupP [combinePats $ map (VarP . mkName) vars, combinePats $ map (VarP . mkName) vars']) (TupE $ map (VarE . mkName) (vars ++ vars'))
interpretFunction (Curry f) = [| curry $(interpretFunction f)|]

data ReindexingExpression = UnitIntroL        -- \x -> ((), x)
                          | UnitIntroR        -- \x -> (x, ())
                          | FlattenTuples Int -- \((...(x1, x2), ...), xn) -> (x1, x2, ..., xn)

reindexExpr :: ReindexingExpression -> Q Exp
reindexExpr UnitIntroL = [| \x -> ((), x) |]
reindexExpr UnitIntroR = [| \x -> (x, ()) |]
reindexExpr (FlattenTuples n) = do names <- traverse (const (newName "x")) [1..n]
                                   let tuples = foldl1 (\a b -> TupE [a, b]) (map VarE names)
                                   pure $ LamE (pure $ TupP $ map VarP names) tuples

data FreeOpenGame = Atom Exp
                  | Lens FunctionExpression FunctionExpression
                  | Function FunctionExpression FunctionExpression
                  | Counit
                  | Sequential FreeOpenGame FreeOpenGame
                  | Simultaneous FreeOpenGame FreeOpenGame
                  | Reindex ReindexingExpression FreeOpenGame

interpretOpenGame :: FreeOpenGame -> Q Exp
interpretOpenGame (Atom n) = pure n
interpretOpenGame (Lens f1 f2) = [| fromLens $(interpretFunction f1) $(interpretFunction f2) |]
interpretOpenGame (Function f1 f2) = [| fromFunctions $(interpretFunction f1) $(interpretFunction f2)|]
interpretOpenGame Counit = [| counit |]
interpretOpenGame (Sequential g1 g2) = [| $(interpretOpenGame g1) >>> $(interpretOpenGame g2)|]
interpretOpenGame (Simultaneous g1 g2) = [| $(interpretOpenGame g1) &&& $(interpretOpenGame g2)|]
interpretOpenGame (Reindex idx game) = [|reindex $(reindexExpr idx) $(interpretOpenGame game)|]


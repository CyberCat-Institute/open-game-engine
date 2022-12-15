{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module OpenGames.Preprocessor.Codegen
    (Variables(..)
    , Expressions(..)
    , FreeOpenGame(..)
    , FunctionExpression(..)
    , interpretOpenGame
    , interpretFunction
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import OpenGames.Preprocessor.RuntimeAST

combinePats :: [Pat] -> Pat
combinePats [x] = x
combinePats xs = TupP xs

apply :: Exp -> [Exp] -> Exp
apply fn [] = fn
apply fn (x : xs) = apply (AppE fn x) xs

mkTup :: [Exp] -> Exp
mkTup [e] = e
mkTup e = TupE (map Just e)

patToExp :: Pat -> Exp
patToExp (VarP e) = VarE e
patToExp (TupP e) = mkTup (map (patToExp) e)
patToExp (LitP e) = LitE e
patToExp (ListP e) = ListE (fmap patToExp e)
patToExp (ConP n t e) = apply (VarE n) (fmap patToExp e)

interpretFunction :: FunctionExpression Pat Exp -> Q Exp
interpretFunction Identity = [| id |]
interpretFunction Copy = [| \x -> (x, x) |]
interpretFunction (Lambda (Variables vars) (Expressions {exps})) =
  pure $ LamE (pure $ combinePats vars) (mkTup exps)
interpretFunction (CopyLambda (Variables [vars]) (Expressions [exps])) =
  pure $ LamE (pure vars) (mkTup [patToExp vars, exps])
interpretFunction (CopyLambda (Variables { vars }) (Expressions [exps])) =
   pure $ LamE (pure $ combinePats vars) (mkTup [mkTup $ map patToExp vars, exps])
interpretFunction (CopyLambda (Variables [vars]) (Expressions { exps })) =
  pure $ LamE (pure vars) (mkTup [patToExp vars, mkTup exps])
interpretFunction (CopyLambda (Variables { vars }) (Expressions { exps })) =
  pure $ LamE (pure $ combinePats vars) (mkTup [mkTup $ map patToExp vars, mkTup exps])
interpretFunction (Multiplex (Variables { vars }) (Variables { vars = vars' })) =
  pure $ LamE (pure $ TupP [combinePats vars, combinePats vars']) (mkTup $ map patToExp (vars ++ vars'))
interpretFunction (Curry f) = [| curry $(interpretFunction f)|]

interpretOpenGame :: FreeOpenGame Pat Exp -> Q Exp
interpretOpenGame (Atom n) = pure n
interpretOpenGame (Lens f1 f2) = [| lift (lens $(interpretFunction f1) $(interpretFunction f2))|]
interpretOpenGame (Function f1 f2) = [| fromFunctions $(interpretFunction f1) $(interpretFunction f2)|]
interpretOpenGame Counit = [| counit |]
interpretOpenGame (Sequential g1 g2) = [| $(interpretOpenGame g1) >>> $(interpretOpenGame g2)|]
interpretOpenGame (Simultaneous g1 g2) = [| $(interpretOpenGame g1) &&& $(interpretOpenGame g2)|]

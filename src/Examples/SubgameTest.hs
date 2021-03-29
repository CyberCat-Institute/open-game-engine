{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Examples.SubgameTest where

-- import Preprocessor.AbstractSyntax
import Language.Haskell.TH.Syntax
import Preprocessor.THSyntax
import Preprocessor.AbstractSyntax
import Preprocessor.Types
import Preprocessor.Compile
import Engine.OpenGamesClass
import Engine.SubgamePerfect
import Engine.Diagnostics -- only used for instance Monoid Bool

-- Ultimatum game

ultimatum = [game| || =>>
  (offer1, offer2) | if accepted then offer1 else 0
    <- reindex const (subgamePerfectDecision [(5, 5), (8, 2)]) -< | ;
  accepted | if accepted then offer2 else 0
    <- subgamePerfectDecision [True, False] -< | offer2;
    <<= ||
  |]

{- Example usage
> equilibrium ultimatum void ((8, 2), const True)
True
> equilibrium ultimatum void ((5, 5), \x -> case x of {5 -> True; 2 -> False})
False
> equilibrium ultimatum void ((8, 2), \x -> case x of {5 -> False; 2 -> True})
False
-}

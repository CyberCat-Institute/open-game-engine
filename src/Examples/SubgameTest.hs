{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module OpenGames.Examples.SubgameTest where

-- import OpenGames.Preprocessor.AbstractSyntax
import Language.Haskell.TH.Syntax
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Types
import OpenGames.Preprocessor.Compile
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.SubgamePerfect
import OpenGames.Engine.Diagnostics -- only used for instance Monoid Bool

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

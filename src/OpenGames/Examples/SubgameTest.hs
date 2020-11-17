{-# LANGUAGE TemplateHaskell #-}

module OpenGames.Examples.SubgameTest where

-- import OpenGames.Preprocessor.AbstractSyntax
import Language.Haskell.TH.Syntax
import OpenGames.Preprocessor.THSyntax
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.SubgamePerfect
import OpenGames.Engine.Diagnostics -- only used for instance Monoid Bool

-- Ultimatum game


-- TODO: covariant inputs should be patterns instead of strings
-- generateGame "ultimatumTH" []
--                      [QLine [] [] [|reindex const (subgamePerfectDecision [(5, 5), (8, 2)])|] ["(offer1, offer2)"] [[|if accepted then offer1 else 0|]]
--                      ,QLine [param "offer2"] [] [|subgamePerfectDecision [True, False]|] ["accepted"] [[|if accepted then offer2 else 0|]]]

ultimatum =
  reindex (\x -> (x, ()))
    ((reindex (\x -> ((), x))
      ((fromFunctions (\x -> x) (\((offer1, offer2), accepted) -> ()))
        >>>
          (reindex (\(a1, a2) -> (a1, a2))
            ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\(((offer1, offer2), accepted), ()) -> ((offer1, offer2), accepted))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (subgamePerfectDecision [(5, 5), (8, 2)]))))))) >>> (fromFunctions (\((), (offer1, offer2)) -> (offer1, offer2)) (\((offer1, offer2), accepted) -> (((offer1, offer2), accepted), if accepted then offer1 else 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(offer1, offer2) -> ((offer1, offer2), offer2)) (\(((offer1, offer2), accepted), ()) -> ((offer1, offer2), accepted))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((subgamePerfectDecision [True, False])))))) >>> (fromFunctions (\((offer1, offer2), accepted) -> ((offer1, offer2), accepted)) (\((offer1, offer2), accepted) -> (((offer1, offer2), accepted), if accepted then offer2 else 0))))))))) >>> (fromLens (\((offer1, offer2), accepted) -> ()) (curry (\(((offer1, offer2), accepted), ()) -> ((offer1, offer2), accepted)))))
--
{- Example usage
> equilibrium ultimatum void ((8, 2), const True)
True
> equilibrium ultimatum void ((5, 5), \x -> case x of {5 -> True; 2 -> False})
False
> equilibrium ultimatum void ((8, 2), \x -> case x of {5 -> False; 2 -> True})
False
-}

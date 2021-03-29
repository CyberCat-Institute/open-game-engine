module OpenGames.Examples.Governance.ThirdOption where

import           OpenGames.Engine.BayesianDiagnostics
import           OpenGames.Examples.Governance.ChoosingRandomly (PDMove (..),
                                                                 StagMove (..),
                                                                 pdMatrix,
                                                                 stagHunt,
                                                                 stagMatrix)
import           OpenGames.Preprocessor.AbstractSyntax

switch :: Maybe PDMove -> Maybe PDMove -> Either (PDMove, PDMove) ()
switch (Just x) (Just y) = Left (x, y)
switch _ _               = Right ()

thirdOptionGameSrc = Block [] []
  [Line [] [] "decision \"player1\" [Just Cooperate, Just Defect, Nothing]" ["move1"] ["payoff1"],
   Line [] [] "decision \"player2\" [Just Cooperate, Just Defect, Nothing]" ["move2"] ["payoff2"],
   Line ["switch move1 move2"] ["payoff1", "payoff2"] "noContinuationSubgame +++ continuationSubgame" [] []]
  [] []

thirdOption = undefined

noContinuationSubgameSrc = Block ["move1", "move2"] ["payoff1", "payoff2"]
  [Line ["move1", "move2"] [] "fromFunctions (uncurry pdMatrix) id" ["payoff1", "payoff2"] []]
  [] []

noContinuationSubgame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(move1, move2, payoff1, payoff2) -> (payoff1, payoff2))) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(move1, move2) -> ((move1, move2), (move1, move2))) (\((move1, move2, payoff1, payoff2), ()) -> (move1, move2, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions (uncurry pdMatrix) id)))))) >>> (fromFunctions (\((move1, move2), (payoff1, payoff2)) -> (move1, move2, payoff1, payoff2)) (\(move1, move2, payoff1, payoff2) -> ((move1, move2, payoff1, payoff2), ())))))))) >>> (fromLens (\(move1, move2, payoff1, payoff2) -> ()) (curry (\((move1, move2, payoff1, payoff2), ()) -> (move1, move2, payoff1, payoff2)))))

{-
continuationSubgameSrc = Block [] ["payoff1", "payoff2"]
  [Line [] [] "decision \"player1\" []"]
  [] []
-}

continuationSubgame = stagHunt

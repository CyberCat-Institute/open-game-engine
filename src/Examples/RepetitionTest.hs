{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Examples.RepetitionTest where

import Engine.PureOpenGames
import Engine.OpenGamesClass
import Engine.Diagnostics
import Preprocessor.THSyntax
import Preprocessor.Types
import Preprocessor.Compile
import Preprocessor.AbstractSyntax

data StagePDTitForTatState = TitForTatState1 | TitForTatState2 deriving (Show)
data StagePDGrimTriggerState = GrimTriggerState1 | GrimTriggerState2 deriving (Show)
data StagePDMove = Cooperate | Defect deriving (Show)

stagePDTitForTatTransition :: StagePDTitForTatState -> StagePDMove -> StagePDTitForTatState
stagePDTitForTatTransition _ Cooperate = TitForTatState1
stagePDTitForTatTransition _ Defect = TitForTatState2

stagePDGrimTriggerTransition :: StagePDGrimTriggerState -> StagePDMove -> StagePDGrimTriggerState
stagePDGrimTriggerTransition GrimTriggerState1 Cooperate = GrimTriggerState1
stagePDGrimTriggerTransition _ _ = GrimTriggerState2

stagePDPayoffs :: StagePDMove -> StagePDMove -> (Double, Double)
stagePDPayoffs Cooperate Cooperate = (2, 2)
stagePDPayoffs Cooperate Defect = (0, 3)
stagePDPayoffs Defect Cooperate = (3, 0)
stagePDPayoffs Defect Defect = (1, 1)

repetitionTestDiscountFactor :: Double
repetitionTestDiscountFactor = 0.1

-- stagePDQQ = [game|
--   payoff1, payoff2 || titForTatState, grimTriggerState =>>
--
--   move1 | payoff1 <- pureDecision [Cooperate, Defect] -< | titForTatState ;
--   move2 | payoff2 <- pureDecision [Cooperate, Defect] -< | grimTriggerState ;
--         | move1, move2 , continuation1, continuation2
--           <- fromFunctions id (\(move1, move2, continuation1, continuation2) ->
--                let (u1, u2) = stagePDPayoffs move1 move2 in (u1 + repetitionTestDiscountFactor*continuation1,
--                                                              u2 + repetitionTestDiscountFactor*continuation2))
--           -< | payoff1, payoff2 ;
--   <<=  (continuation1, continuation2)
--   || (stagePDTitForTatTransition titForTatState move2, stagePDGrimTriggerTransition grimTriggerState move1), (move1, move2)
--   |]

-- generateGame "stagePDTH" [] $  GBlock ["titForTatState", "grimTriggerState"] ["payoff1", "payoff2"]
--   [Line [param "titForTatState"]   [] [|pureDecision [Cooperate, Defect]|] ["move1"] [[|payoff1|]],
--    Line [param "grimTriggerState"] [] [|pureDecision [Cooperate, Defect]|] ["move2"] [[|payoff2|]],
--    Line [] ["payoff1", "payoff2"]
--          [|fromFunctions id (\(move1, move2, continuation1, continuation2) ->
--            let (u1, u2) = stagePDPayoffs move1 move2 in (u1 + repetitionTestDiscountFactor*continuation1,
--                                                          u2 + repetitionTestDiscountFactor*continuation2))|]
--         [] [[|move1|], [|move2|], [|continuation1|], [|continuation2|]]]
--   ["(stagePDTitForTatTransition titForTatState move2, stagePDGrimTriggerTransition grimTriggerState move1)", "(move1, move2)"]
--   ["(continuation1, continuation2)"]

stagePD = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2) -> (payoff1, payoff2))) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(titForTatState, grimTriggerState) -> ((titForTatState, grimTriggerState), titForTatState)) (\((titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2), ()) -> (titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((pureDecision [Cooperate, Defect])))))) >>> (fromFunctions (\((titForTatState, grimTriggerState), move1) -> (titForTatState, grimTriggerState, move1)) (\(titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2) -> ((titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2), payoff1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(titForTatState, grimTriggerState, move1) -> ((titForTatState, grimTriggerState, move1), grimTriggerState)) (\((titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2), ()) -> (titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((pureDecision [Cooperate, Defect])))))) >>> (fromFunctions (\((titForTatState, grimTriggerState, move1), move2) -> (titForTatState, grimTriggerState, move1, move2)) (\(titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2) -> ((titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2), payoff2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(titForTatState, grimTriggerState, move1, move2) -> ((titForTatState, grimTriggerState, move1, move2), ())) (\((titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2)), (payoff1, payoff2)) -> (titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2), payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions id (\(move1, move2, continuation1, continuation2) ->  let (u1, u2) = stagePDPayoffs move1 move2 in (u1 + repetitionTestDiscountFactor*continuation1,  u2 + repetitionTestDiscountFactor*continuation2)))))))) >>> (fromFunctions (\((titForTatState, grimTriggerState, move1, move2), ()) -> (titForTatState, grimTriggerState, move1, move2)) (\(titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2)) -> ((titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2)), (move1, move2, continuation1, continuation2)))))))))) >>> (fromLens (\(titForTatState, grimTriggerState, move1, move2) -> ((stagePDTitForTatTransition titForTatState move2, stagePDGrimTriggerTransition grimTriggerState move1), (move1, move2))) (curry (\((titForTatState, grimTriggerState, move1, move2), (continuation1, continuation2)) -> (titForTatState, grimTriggerState, move1, move2, (continuation1, continuation2))))))

repeatedPDStates = [(a, b) | a <- [TitForTatState1, TitForTatState2], b <- [GrimTriggerState1, GrimTriggerState2]]

generateGame "repeatedPDTH" [] $
  [Line [[|TitForTatState1|], [|GrimTriggerState1|]] ["payoff1", "payoff2"] [|repeated 10 (0, 0) repeatedPDStates stagePD|] ["moves"] []]

repeatedPD = [game| || =>>
  moves | <- repeated 10 (0, 0) repeatedPDStates stagePD -< payoff1, payoff2 | TitForTatState1, GrimTriggerState1 ;
  <<= || |]

repeatedPDTitForTat TitForTatState1 = Cooperate
repeatedPDTitForTat TitForTatState2 = Defect
repeatedPDGrimTrigger GrimTriggerState1 = Cooperate
repeatedPDGrimTrigger GrimTriggerState2 = Defect

repeatedPDTest = equilibrium repeatedPD () id

{- After all that, it gives the wrong answer
> repeatedPDTest (\x -> case x of {TitForTatState1 -> Cooperate; TitForTatState2 -> Defect}) (\x -> case x of {GrimTriggerState1 -> Cooperate; GrimTriggerState2 -> Defect})
False
-}

{- More data:
> let k (x, _) = Engine.PureOpenGames.coplay (repeated 10 (0, 0) repeatedPDStates stagePD) (repeatedPDTitForTat, repeatedPDGrimTrigger, ()) x ()
> [((s1, s2), Engine.PureOpenGames.equilibrium stagePD (s1, s2) k (repeatedPDTitForTat, repeatedPDGrimTrigger, ())) | s1 <- [TitForTatState1, TitForTatState2], s2 <- [GrimTriggerState1, GrimTriggerState2]]
[((TitForTatState1,GrimTriggerState1),False),((TitForTatState1,GrimTriggerState2),False),((TitForTatState2,GrimTriggerState1),False),((TitForTatState2,GrimTriggerState2),True)]
-}

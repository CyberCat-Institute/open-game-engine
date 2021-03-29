module Examples.Governance.ChoosingRandomly where

import Numeric.Probability.Distribution
import Preprocessor.AbstractSyntax
import Engine.BayesianDiagnostics

majority2 :: (Fractional prob) => Either () () -> Either () () -> T prob (Either () ())
majority2 (Left ()) (Left ()) = certainly (Left ())
majority2 (Right ()) (Right ()) = certainly (Right ())
majority2 _ _ = uniform [Left (), Right ()]

data PDMove = Cooperate | Defect deriving (Eq, Ord, Show)
pdMatrix :: PDMove -> PDMove -> (Rational, Rational)
pdMatrix Cooperate Cooperate = (2, 2)
pdMatrix Defect Defect       = (1, 1)
pdMatrix Cooperate Defect    = (0, 3)
pdMatrix Defect Cooperate    = (3, 0)

prisonersDilemmaSrc = Block [] ["pdMatrix move1 move2"]
                            [Line [] [] "reindex const (decision \"player1\" [Cooperate, Defect])" ["move1"] ["fst (pdMatrix move1 move2)"],
                             Line [] [] "reindex const (decision \"player2\" [Cooperate, Defect])" ["move2"] ["snd (pdMatrix move1 move2)"]]
                            [] []

prisonersDilemma = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(move1, move2) -> pdMatrix move1 move2)) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((move1, move2), ()) -> (move1, move2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Cooperate, Defect]))))))) >>> (fromFunctions (\((), move1) -> move1) (\(move1, move2) -> ((move1, move2), fst (pdMatrix move1 move2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\move1 -> (move1, ())) (\((move1, move2), ()) -> (move1, move2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Cooperate, Defect]))))))) >>> (fromFunctions (\(move1, move2) -> (move1, move2)) (\(move1, move2) -> ((move1, move2), snd (pdMatrix move1 move2)))))))))) >>> (fromLens (\(move1, move2) -> ()) (curry (\((move1, move2), ()) -> (move1, move2)))))

data StagMove = Stag | Hare deriving (Eq, Ord, Show)
stagMatrix :: StagMove -> StagMove -> (Rational, Rational)
stagMatrix Stag Stag = (2, 2)
stagMatrix Stag Hare = (0, 1)
stagMatrix Hare Stag = (1, 0)
stagMatrix Hare Hare = (1, 1)

stagHuntSrc = Block [] ["stagMatrix move1 move2"]
                            [Line [] [] "reindex const (decision \"player1\" [Stag, Hare])" ["move1"] ["fst (stagMatrix move1 move2)"],
                             Line [] [] "reindex const (decision \"player2\" [Stag, Hare])" ["move2"] ["snd (stagMatrix move1 move2)"]]
                            [] []

stagHunt = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(move1, move2) -> stagMatrix move1 move2)) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((move1, move2), ()) -> (move1, move2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Stag, Hare]))))))) >>> (fromFunctions (\((), move1) -> move1) (\(move1, move2) -> ((move1, move2), fst (stagMatrix move1 move2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\move1 -> (move1, ())) (\((move1, move2), ()) -> (move1, move2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Stag, Hare]))))))) >>> (fromFunctions (\(move1, move2) -> (move1, move2)) (\(move1, move2) -> ((move1, move2), snd (stagMatrix move1 move2)))))))))) >>> (fromLens (\(move1, move2) -> ()) (curry (\((move1, move2), ()) -> (move1, move2)))))

metagameSrc = Block [] []
                    [Line [] [] "reindex const (decision \"player1\" [Left (), Right ()])" ["vote1"] ["payoff1"],
                     Line [] [] "reindex const (decision \"player2\" [Left (), Right ()])" ["vote2"] ["payoff2"],
                     Line ["vote1", "vote2"] [] "liftStochastic (uncurry majority2)" ["result"] [],
                     Line ["result"] ["payoff1", "payoff2"] "prisonersDilemma +++ stagHunt" ["discard"] []]
                    [] []

metagame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2, result, discard, payoff1, payoff2), ()) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Left (), Right ()]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ((vote1, vote2, result, discard, payoff1, payoff2), payoff1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2, result, discard, payoff1, payoff2), ()) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Left (), Right ()]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ((vote1, vote2, result, discard, payoff1, payoff2), payoff2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2) -> ((vote1, vote2), (vote1, vote2))) (\((vote1, vote2, result, discard, payoff1, payoff2), ()) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((liftStochastic (uncurry majority2))))))) >>> (fromFunctions (\((vote1, vote2), result) -> (vote1, vote2, result)) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ((vote1, vote2, result, discard, payoff1, payoff2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2, result) -> ((vote1, vote2, result), result)) (\((vote1, vote2, result, discard), (payoff1, payoff2)) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((prisonersDilemma +++ stagHunt)))))) >>> (fromFunctions (\((vote1, vote2, result), discard) -> (vote1, vote2, result, discard)) (\(vote1, vote2, result, discard) -> ((vote1, vote2, result, discard), ()))))))))) >>> (fromLens (\(vote1, vote2, result, discard) -> ()) (curry (\((vote1, vote2, result, discard), ()) -> (vote1, vote2, result, discard)))))

randomMetagameEq = equilibrium metagame trivialContext

{- Example usage
> :t randomMetagameEq 
randomMetagameEq
  :: (T Rational (Either () ()), T Rational (Either () ()), (),
      ((T Rational Examples.Governance.ChoosingRandomly.PDMove,
        T Rational Examples.Governance.ChoosingRandomly.PDMove),
       (T Rational StagMove, T Rational StagMove)))
     -> [Engine.BayesianDiagnostics.DiagnosticInfo]
> randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Defect, certainly Defect), (certainly Stag, certainly Stag)))
[]
> randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Defect, certainly Cooperate), (certainly Stag, certainly Stag)))
[DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "((),())", strategy = "fromFreqs [(Right (),1 % 1)]", payoff = "2 % 1", optimalMove = "Left ()", optimalPayoff = "5 % 2"}]
> randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
[]
> :r -- parameters changed here
[38 of 38] Compiling Examples.Governance.ChoosingRandomly ( /Users/jules/Desktop/Git/open-games-hs-private/src/Examples/Governance/ChoosingRandomly.hs, interpreted )
Ok, 38 modules loaded.
> randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Defect, certainly Defect), (certainly Stag, certainly Stag)))
[]
> randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
[DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "((),())", strategy = "fromFreqs [(Right (),1 % 1)]", payoff = "2 % 1", optimalMove = "Left ()", optimalPayoff = "5 % 2"},DiagnosticInfo {player = "player2", observedState = "()", unobservedState = "((),Right ())", strategy = "fromFreqs [(Right (),1 % 1)]", payoff = "2 % 1", optimalMove = "Left ()", optimalPayoff = "5 % 2"}]
> randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Defect, certainly Cooperate), (certainly Stag, certainly Stag)))
[DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "((),())", strategy = "fromFreqs [(Right (),1 % 1)]", payoff = "2 % 1", optimalMove = "Left ()", optimalPayoff = "3 % 1"}]
> randomMetagameEq (certainly (Left ()), certainly (Left ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
[DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "(((),(Left (),Left (),Left ())),())", strategy = "fromFreqs [(Cooperate,1 % 1)]", payoff = "3 % 1", optimalMove = "Defect", optimalPayoff = "4 % 1"},DiagnosticInfo {player = "player2", observedState = "()", unobservedState = "(((),(Left (),Left (),Left ())),Cooperate)", strategy = "fromFreqs [(Cooperate,1 % 1)]", payoff = "3 % 1", optimalMove = "Defect", optimalPayoff = "4 % 1"}]
> mapM_ print $ randomMetagameEq (certainly (Left ()), certainly (Left ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "(((),(Left (),Left (),Left ())),())", strategy = "fromFreqs [(Cooperate,1 % 1)]", payoff = "3 % 1", optimalMove = "Defect", optimalPayoff = "4 % 1"}
DiagnosticInfo {player = "player2", observedState = "()", unobservedState = "(((),(Left (),Left (),Left ())),Cooperate)", strategy = "fromFreqs [(Cooperate,1 % 1)]", payoff = "3 % 1", optimalMove = "Defect", optimalPayoff = "4 % 1"}
> mapM_ print $ randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "((),())", strategy = "fromFreqs [(Right (),1 % 1)]", payoff = "2 % 1", optimalMove = "Left ()", optimalPayoff = "5 % 2"}
DiagnosticInfo {player = "player2", observedState = "()", unobservedState = "((),Right ())", strategy = "fromFreqs [(Right (),1 % 1)]", payoff = "2 % 1", optimalMove = "Left ()", optimalPayoff = "5 % 2"}
> :r -- parameters changed back here
[38 of 38] Compiling Examples.Governance.ChoosingRandomly ( /Users/jules/Desktop/Git/open-games-hs-private/src/Examples/Governance/ChoosingRandomly.hs, interpreted )
Ok, 38 modules loaded.
> mapM_ print $ randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
> mapM_ print $ randomMetagameEq (certainly (Left ()), certainly (Left ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "(((),(Left (),Left (),Left ())),())", strategy = "fromFreqs [(Cooperate,1 % 1)]", payoff = "2 % 1", optimalMove = "Defect", optimalPayoff = "3 % 1"}
DiagnosticInfo {player = "player2", observedState = "()", unobservedState = "(((),(Left (),Left (),Left ())),Cooperate)", strategy = "fromFreqs [(Cooperate,1 % 1)]", payoff = "2 % 1", optimalMove = "Defect", optimalPayoff = "3 % 1"}
> mapM_ print $ randomMetagameEq (certainly (Left ()), certainly (Left ()), (), ((certainly Defect, certainly Defect), (certainly Stag, certainly Stag)))
DiagnosticInfo {player = "player1", observedState = "()", unobservedState = "((),())", strategy = "fromFreqs [(Left (),1 % 1)]", payoff = "1 % 1", optimalMove = "Right ()", optimalPayoff = "3 % 2"}
DiagnosticInfo {player = "player2", observedState = "()", unobservedState = "((),Left ())", strategy = "fromFreqs [(Left (),1 % 1)]", payoff = "1 % 1", optimalMove = "Right ()", optimalPayoff = "3 % 2"}
> mapM_ print $ randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Defect, certainly Defect), (certainly Stag, certainly Stag)))
> mapM_ print $ randomMetagameEq (certainly (Right ()), certainly (Right ()), (), ((certainly Cooperate, certainly Cooperate), (certainly Stag, certainly Stag)))
-}

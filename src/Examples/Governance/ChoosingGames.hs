module Examples.Governance.ChoosingGames where

import Data.List (sort)

import Examples.Bimatrix (Coin(..), matchingPenniesMatrix1, matchingPenniesMatrix2, Coordination(..), meetingInNYMatrix)
import Preprocessor.AbstractSyntax
import Engine.OpenGamesClass
import Engine.StatefulPayoffs

matchingPenniesSrc = Block [] []
                           [Line [] [] "reindex const (agentDecision \"player1\" [Heads, Tails])" ["x"] ["matchingPenniesMatrix1 x y"],
                            Line [] [] "reindex const (agentDecision \"player2\" [Heads, Tails])" ["y"] ["matchingPenniesMatrix2 x y"]]
                           [] []

matchingPennies = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player1" [Heads, Tails]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), matchingPenniesMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player2" [Heads, Tails]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), matchingPenniesMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

meetingInNYSrc = Block [] []
                           [Line [] [] "reindex const (agentDecision \"player1\" [GCT, ES])" ["x"] ["meetingInNYMatrix x y"],
                            Line [] [] "reindex const (agentDecision \"player2\" [GCT, ES])" ["y"] ["meetingInNYMatrix x y"]]
                           [] []


meetingInNY = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player1" [GCT, ES]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), meetingInNYMatrix x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player2" [GCT, ES]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), meetingInNYMatrix x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

data PrisonersDilemmaMove = Cooperate | Defect deriving (Eq, Ord, Show)

prisonersDilemmaPayoffs :: PrisonersDilemmaMove -> PrisonersDilemmaMove -> (Rational, Rational)
prisonersDilemmaPayoffs Cooperate Cooperate = (2, 2)
prisonersDilemmaPayoffs Cooperate Defect    = (0, 3)
prisonersDilemmaPayoffs Defect Cooperate    = (3, 0)
prisonersDilemmaPayoffs Defect Defect       = (1, 1)

prisonersDilemmaSrc = Block [] []
                           [Line [] [] "reindex const (agentDecision \"player1\" [Cooperate, Defect])" ["x"] ["fst (prisonersDilemmaPayoffs x y)"],
                            Line [] [] "reindex const (agentDecision \"player2\" [Cooperate, Defect])" ["y"] ["snd (prisonersDilemmaPayoffs x y)"]]
                           [] []

prisonersDilemma = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player1" [Cooperate, Defect]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), fst (prisonersDilemmaPayoffs x y)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player2" [Cooperate, Defect]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), snd (prisonersDilemmaPayoffs x y)))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

metagameSrc = Block [] []
                    [Line [] [] "reindex const (agentDecision \"player1\" [Left (), Right ()])" ["branch"] ["0"],
                     Line ["branch"] [] "prisonersDilemma +++ meetingInNY" ["discard"] []]
                    [] []

metagame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(branch, discard) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((branch, discard), ()) -> (branch, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player1" [Left (), Right ()]))))))) >>> (fromFunctions (\((), branch) -> branch) (\(branch, discard) -> ((branch, discard), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\branch -> (branch, branch)) (\((branch, discard), ()) -> (branch, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((prisonersDilemma +++ meetingInNY)))))) >>> (fromFunctions (\(branch, discard) -> (branch, discard)) (\(branch, discard) -> ((branch, discard), ()))))))))) >>> (fromLens (\(branch, discard) -> ()) (curry (\((branch, discard), ()) -> (branch, discard)))))

metagameEq = equilibrium metagame () (const (return ()))

{- Example usage:
> metagameEq (Left (), ((Defect, Defect), (ES, GCT)))
[]
> metagameEq (Left (), ((Cooperate, Cooperate), (ES, GCT)))
[DiagnosticInfo {player = "\"player1\"", state = "()", strategy = "Cooperate", payoff = "2 % 1", optimalMove = "Defect", optimalPayoff = "3 % 1"},DiagnosticInfo {player = "\"player2\"", state = "()", strategy = "Cooperate", payoff = "2 % 1", optimalMove = "Defect", optimalPayoff = "3 % 1"}]
> metagameEq (Right (), ((Defect, Defect), (ES, ES)))
[]
> metagameEq (Right (), ((Cooperate, Cooperate), (ES, ES)))
[DiagnosticInfo {player = "\"player1\"", state = "()", strategy = "Right ()", payoff = "1 % 1", optimalMove = "Left ()", optimalPayoff = "2 % 1"}]
> metagameEq (Right (), ((Defect, Cooperate), (ES, ES)))
[DiagnosticInfo {player = "\"player1\"", state = "()", strategy = "Right ()", payoff = "1 % 1", optimalMove = "Left ()", optimalPayoff = "3 % 1"}]
> metagameEq (Right (), ((Cooperate, Defect), (ES, ES)))
[]
-}

prisonersDilemmaPenaltySrc = Block ["penalty"] []
                           [Line ["penalty"] [] "reindex const (agentDecision \"player1\" [Cooperate, Defect])" ["x"] ["fst (prisonersDilemmaPayoffs x y) - if x == Defect then penalty else 0"],
                            Line ["penalty"] [] "reindex const (agentDecision \"player2\" [Cooperate, Defect])" ["y"] ["snd (prisonersDilemmaPayoffs x y) - if y == Defect then penalty else 0"]]
                           [] []

prisonersDilemmaPenalty = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(penalty, x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\penalty -> (penalty, penalty)) (\((penalty, x, y), ()) -> (penalty, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player1" [Cooperate, Defect]))))))) >>> (fromFunctions (\(penalty, x) -> (penalty, x)) (\(penalty, x, y) -> ((penalty, x, y), fst (prisonersDilemmaPayoffs x y) - if x == Defect then penalty else 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(penalty, x) -> ((penalty, x), penalty)) (\((penalty, x, y), ()) -> (penalty, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player2" [Cooperate, Defect]))))))) >>> (fromFunctions (\((penalty, x), y) -> (penalty, x, y)) (\(penalty, x, y) -> ((penalty, x, y), snd (prisonersDilemmaPayoffs x y) - if y == Defect then penalty else 0))))))))) >>> (fromLens (\(penalty, x, y) -> ()) (curry (\((penalty, x, y), ()) -> (penalty, x, y)))))

prisonersDilemmaMetagameSrc = Block [] []
                                    [Line [] [] "reindex const (agentDecision dictator [0..3])" ["penalty"] ["0"],
                                     Line ["penalty"] [] "prisonersDilemmaPenalty" [] []]
                                    [] []

prisonersDilemmaMetagame dictator = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\penalty -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\(penalty, ()) -> penalty)) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision dictator [0..3]))))))) >>> (fromFunctions (\((), penalty) -> penalty) (\penalty -> (penalty, 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\penalty -> (penalty, penalty)) (\(penalty, ()) -> penalty)) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((prisonersDilemmaPenalty)))))) >>> (fromFunctions (\(penalty, ()) -> penalty) (\penalty -> (penalty, ()))))))))) >>> (fromLens (\penalty -> ()) (curry (\(penalty, ()) -> penalty))))

prisonersDilemmaMetagameEq = equilibrium (prisonersDilemmaMetagame "player1") () (const (return ()))

-- Idea for next week: 3rd player votes to break tie but doesn't take part in the real game

prisonersDilemmaMetagame2Src = Block ["dictator"] []
                                     [Line ["dictator", "()"] [] "reindex const (roleDecision [0..3])" ["penalty"] ["1"],
                                      Line ["penalty"] [] "prisonersDilemmaPenalty" [] []]
                                     [] []

prisonersDilemmaMetagame2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(dictator, penalty) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\dictator -> (dictator, (dictator, ()))) (\((dictator, penalty), ()) -> (dictator, penalty))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (roleDecision [0..3]))))))) >>> (fromFunctions (\(dictator, penalty) -> (dictator, penalty)) (\(dictator, penalty) -> ((dictator, penalty), 1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(dictator, penalty) -> ((dictator, penalty), penalty)) (\((dictator, penalty), ()) -> (dictator, penalty))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((prisonersDilemmaPenalty)))))) >>> (fromFunctions (\((dictator, penalty), ()) -> (dictator, penalty)) (\(dictator, penalty) -> ((dictator, penalty), ()))))))))) >>> (fromLens (\(dictator, penalty) -> ()) (curry (\((dictator, penalty), ()) -> (dictator, penalty)))))

majority :: String -> String -> String -> String
majority a b c = let [_, x, _] = sort [a, b, c] in x

prisonersDilemmaVotingMetagameSrc = Block [] []
                                          [Line [] [] "reindex const (agentDecision \"player1\" [\"player1\", \"player2\"])" ["vote1"] ["0"],
                                           Line [] [] "reindex const (agentDecision \"player2\" [\"player1\", \"player2\"])" ["vote2"] ["0"],
                                           Line [] [] "reindex const (agentDecision \"player3\" [\"player1\", \"player2\"])" ["vote3"] ["0"],
                                           Line ["majority vote1 vote2 vote3"] [] "prisonersDilemmaMetagame2" [] []]
                                           [] []

prisonersDilemmaVotingMetagame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2, vote3) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2, vote3), ()) -> (vote1, vote2, vote3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player1" ["player1", "player2"]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2, vote3) -> ((vote1, vote2, vote3), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2, vote3), ()) -> (vote1, vote2, vote3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player2" ["player1", "player2"]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2, vote3) -> ((vote1, vote2, vote3), 0)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2) -> ((vote1, vote2), ())) (\((vote1, vote2, vote3), ()) -> (vote1, vote2, vote3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (agentDecision "player3" ["player1", "player2"]))))))) >>> (fromFunctions (\((vote1, vote2), vote3) -> (vote1, vote2, vote3)) (\(vote1, vote2, vote3) -> ((vote1, vote2, vote3), 0)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2, vote3) -> ((vote1, vote2, vote3), majority vote1 vote2 vote3)) (\((vote1, vote2, vote3), ()) -> (vote1, vote2, vote3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((prisonersDilemmaMetagame2)))))) >>> (fromFunctions (\((vote1, vote2, vote3), ()) -> (vote1, vote2, vote3)) (\(vote1, vote2, vote3) -> ((vote1, vote2, vote3), ()))))))))) >>> (fromLens (\(vote1, vote2, vote3) -> ()) (curry (\((vote1, vote2, vote3), ()) -> (vote1, vote2, vote3)))))

prisonersDilemmaVotingMetagameEq = equilibrium prisonersDilemmaVotingMetagame () (const $ return ())

{- Example usage
> :m - Examples.EcologicalPublicGood.EcologicalPublicGood 
> prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 0, (Defect, Defect)))
[]
> prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 0, (Cooperate, Cooperate)))
[DiagnosticInfo {player = "\"player1\"", state = "0 % 1", strategy = "Cooperate", payoff = "2 % 1", optimalMove = "Defect", optimalPayoff = "3 % 1"},DiagnosticInfo {player = "\"player2\"", state = "0 % 1", strategy = "Cooperate", payoff = "2 % 1", optimalMove = "Defect", optimalPayoff = "3 % 1"}]
> prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 3, (Cooperate, Cooperate)))
[]
> prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 3, (Defect, Defect)))
[DiagnosticInfo {player = "\"player1\"", state = "()", strategy = "3 % 1", payoff = "(-2) % 1", optimalMove = "0 % 1", optimalPayoff = "1 % 1"},DiagnosticInfo {player = "\"player1\"", state = "3 % 1", strategy = "Defect", payoff = "(-2) % 1", optimalMove = "Cooperate", optimalPayoff = "0 % 1"},DiagnosticInfo {player = "\"player2\"", state = "3 % 1", strategy = "Defect", payoff = "(-2) % 1", optimalMove = "Cooperate", optimalPayoff = "0 % 1"}]
> mapM_ print $ prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 3, (Defect, Defect)))
DiagnosticInfo {player = "\"player1\"", state = "()", strategy = "3 % 1", payoff = "(-2) % 1", optimalMove = "0 % 1", optimalPayoff = "1 % 1"}
DiagnosticInfo {player = "\"player1\"", state = "3 % 1", strategy = "Defect", payoff = "(-2) % 1", optimalMove = "Cooperate", optimalPayoff = "0 % 1"}
DiagnosticInfo {player = "\"player2\"", state = "3 % 1", strategy = "Defect", payoff = "(-2) % 1", optimalMove = "Cooperate", optimalPayoff = "0 % 1"}
> mapM_ print $ prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 1, (Defect, Defect)))
DiagnosticInfo {player = "\"player1\"", state = "()", strategy = "1 % 1", payoff = "0 % 1", optimalMove = "0 % 1", optimalPayoff = "1 % 1"}
> mapM_ print $ prisonersDilemmaVotingMetagameEq ("player1", "player2", "player1", (const 1, (Cooperate, Cooperate)))
> mapM_ print $ prisonersDilemmaVotingMetagameEq ("player1", "player1", "player1", (const 1, (Cooperate, Cooperate)))
-}

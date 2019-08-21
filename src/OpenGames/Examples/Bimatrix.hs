module OpenGames.Examples.Bimatrix where

import Numeric.Probability.Distribution

import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.BayesianOpenGames

-- Matching pennies

data Coin = Heads | Tails deriving (Eq, Ord, Show)

matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Rational
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1

matchingPennies = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (bayesianDecisionDiagnostic "player1" [Heads, Tails]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), matchingPenniesMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (bayesianDecisionDiagnostic "player2" [Heads, Tails]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), matchingPenniesMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

matchingPenniesEquilibrium = equilibrium matchingPennies trivialContext

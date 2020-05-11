module OpenGames.Examples.Bimatrix where

import OpenGames.Engine.BayesianDiagnostics
import Numeric.Probability.Distribution
import qualified OpenGames.Engine.BayesianDiagnosticsTLL as TLL

-- Matching pennies

data Coin = Heads | Tails deriving (Eq, Ord, Show)

matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Rational
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1

matchingPennies = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Heads, Tails]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), matchingPenniesMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Heads, Tails]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), matchingPenniesMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

matchingPenniesEquilibrium = equilibrium matchingPennies trivialContext

-- Meeting in New York

data Coordination = GCT | ES deriving (Eq, Ord, Show)
meetingInNYMatrix :: Coordination -> Coordination -> Rational
meetingInNYMatrix x y = if x == y then 1 else 0

meetingInNY = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [GCT, ES]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), meetingInNYMatrix x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [GCT, ES]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), meetingInNYMatrix x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

meetingInNYEquilibrium = equilibrium meetingInNY trivialContext

-- Adding a third player

meetingInNYMatrix3 :: Coordination -> Coordination -> Coordination -> Rational
meetingInNYMatrix3 x y z = if x == y && x == z then 1 else 0

meetingInNY3 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y, z) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y, z), ()) -> (x, y, z))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [GCT, ES]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y, z) -> ((x, y, z), meetingInNYMatrix3 x y z))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y, z), ()) -> (x, y, z))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [GCT, ES]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y, z) -> ((x, y, z), meetingInNYMatrix3 x y z)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(x, y) -> ((x, y), ())) (\((x, y, z), ()) -> (x, y, z))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player3" [GCT, ES]))))))) >>> (fromFunctions (\((x, y), z) -> (x, y, z)) (\(x, y, z) -> ((x, y, z), meetingInNYMatrix3 x y z))))))))) >>> (fromLens (\(x, y, z) -> ()) (curry (\((x, y, z), ()) -> (x, y, z)))))

meetingInNYEquilibrium3 = equilibrium meetingInNY3 trivialContext

-- Prisoner's dilemma

data Prisoner = Cooperate | Defect deriving (Eq, Ord, Show)
prisonersDilemmaMatrix1, prisonersDilemmaMatrix2 :: Prisoner -> Prisoner -> Rational
prisonersDilemmaMatrix1 x y = if x == Cooperate && y == Cooperate then 1
                              else if x == Cooperate && y == Defect then 3
                              else if x == Defect && y == Cooperate then 0
                              else 2
prisonersDilemmaMatrix2 x y = prisonersDilemmaMatrix1 y x

prisonersDilemma = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Cooperate, Defect]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), prisonersDilemmaMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Cooperate, Defect]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), prisonersDilemmaMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

prisonersDilemmaEquilibrium = equilibrium prisonersDilemma trivialContext

-- Battle of the Sexes

data Venue = Opera | Football deriving (Eq, Ord, Show)
botsMatrix1, botsMatrix2 :: Venue -> Venue -> Rational
botsMatrix1 x y = if x /= y then 0
                  else if x == Opera && y == Opera then 3
                  else 2
botsMatrix2 x y = if x /= y then 0
                  else if x == Opera && y == Opera then 2
                  else 3

bots = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Opera, Football]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), botsMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Opera, Football]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), botsMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

botsEquilibrium = equilibrium bots trivialContext

-- Chicken Game

data Drive = Swerve | Straight deriving (Eq, Ord, Show)
chickenMatrix1, chickenMatrix2 :: Drive -> Drive -> Rational
chickenMatrix1 x y = if x == Swerve && y == Swerve then 0
                     else if x == Swerve && y == Straight then -1
                     else if x == Straight && y == Swerve then 1
                     else -1000
chickenMatrix2 x y = if x == Swerve && y == Swerve then 0
                     else if x == Swerve && y == Straight then 1
                     else if x == Straight && y == Swerve then -1
                     else -1000

chicken = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Swerve, Straight]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), chickenMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Swerve, Straight]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), chickenMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

chickenEquilibrium = equilibrium chicken trivialContext

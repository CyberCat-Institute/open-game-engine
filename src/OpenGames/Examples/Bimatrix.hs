{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module OpenGames.Examples.Bimatrix where

import GHC.Generics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import Language.Haskell.TH.Syntax
import OpenGames.Engine.BayesianDiagnostics
import Numeric.Probability.Distribution
import qualified OpenGames.Engine.BayesianDiagnosticsTLL as TLL

-- Matching pennies

data Coin = Heads | Tails deriving (Eq, Ord, Show)

matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Rational
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1

-- Using TH
generateGame "matchingPenniesTH" []
                        [QLine [] [] [|reindex const (decision "player1" [Heads, Tails])|] ["x"] [[|matchingPenniesMatrix1 x y|]],
                         QLine [] [] [|reindex const (decision "player2" [Heads, Tails])|] ["y"] [[|matchingPenniesMatrix2 x y|]]]

-- Using Blocks
matchingPenniesSrc = Block [] []
                           [Line [] [] "reindex const (decision \"player1\" [Heads, Tails])" ["x"] ["matchingPenniesMatrix1 x y"],
                            Line [] [] "reindex const (decision \"player2\" [Heads, Tails])" ["y"] ["matchingPenniesMatrix2 x y"]]
                           [] []

matchingPennies = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Heads, Tails]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), matchingPenniesMatrix1 x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Heads, Tails]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), matchingPenniesMatrix2 x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

matchingPenniesEquilibrium = equilibrium matchingPennies trivialContext


-- Meeting in New York

data Coordination = GCT | ES deriving (Eq, Ord, Show, Generic)

meetingInNYMatrix :: Coordination -> Coordination -> Rational
meetingInNYMatrix x y = if x == y then 1 else 0

-- Using TH
generateGame "meetingInNYTH" []
                        [QLine [] [] [|reindex const (decision "player1" [GCT, ES])|] ["x"] [[|meetingInNYMatrix x y|]],
                         QLine [] [] [|reindex const (decision "player2" [GCT, ES])|] ["y"] [[|meetingInNYMatrix x y|]]]

-- Using Blocks
meetingInNYsrc = Block [] []
                       [Line [] [] "reindex const (decision \"player1\" [GCT, ES])" ["x"] ["meetingInNYMatrix x y"],
                        Line [] [] "reindex const (decision \"player2\" [GCT, ES])" ["y"] ["meetingInNYMatrix x y"]]
                       [] []


meetingInNY = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [GCT, ES]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y) -> ((x, y), meetingInNYMatrix x y))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y), ()) -> (x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [GCT, ES]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y) -> ((x, y), meetingInNYMatrix x y))))))))) >>> (fromLens (\(x, y) -> ()) (curry (\((x, y), ()) -> (x, y)))))

meetingInNYEquilibrium = equilibrium meetingInNY trivialContext

-- Adding a third player

meetingInNYMatrix3 :: Coordination -> Coordination -> Coordination -> Rational
meetingInNYMatrix3 x y z = if x == y && x == z then 1 else 0

-- Using TH
generateGame "meetingInNY3TH" []
                        [QLine [] [] [|reindex const (decision "player1" [GCT, ES])|] ["x"] [[|meetingInNYMatrix3 x y z|]],
                         QLine [] [] [|reindex const (decision "player2" [GCT, ES])|] ["y"] [[|meetingInNYMatrix3 x y z|]],
                         QLine [] [] [|reindex const (decision "player3" [GCT, ES])|] ["z"] [[|meetingInNYMatrix3 x y z|]]]

-- Using Blocks

meetingInNY3src = Block [] []
                        [Line [] [] "reindex const (decision \"player1\" [GCT, ES])" ["x"] ["meetingInNYMatrix3 x y z"],
                         Line [] [] "reindex const (decision \"player2\" [GCT, ES])" ["y"] ["meetingInNYMatrix3 x y z"],
                         Line [] [] "reindex const (decision \"player3\" [GCT, ES])" ["z"] ["meetingInNYMatrix3 x y z"]]
                        [] []

meetingInNY3 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(x, y, z) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((x, y, z), ()) -> (x, y, z))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [GCT, ES]))))))) >>> (fromFunctions (\((), x) -> x) (\(x, y, z) -> ((x, y, z), meetingInNYMatrix3 x y z))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> (x, ())) (\((x, y, z), ()) -> (x, y, z))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [GCT, ES]))))))) >>> (fromFunctions (\(x, y) -> (x, y)) (\(x, y, z) -> ((x, y, z), meetingInNYMatrix3 x y z)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(x, y) -> ((x, y), ())) (\((x, y, z), ()) -> (x, y, z))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player3" [GCT, ES]))))))) >>> (fromFunctions (\((x, y), z) -> (x, y, z)) (\(x, y, z) -> ((x, y, z), meetingInNYMatrix3 x y z))))))))) >>> (fromLens (\(x, y, z) -> ()) (curry (\((x, y, z), ()) -> (x, y, z)))))

meetingInNYEquilibrium3 = equilibrium meetingInNY3 trivialContext

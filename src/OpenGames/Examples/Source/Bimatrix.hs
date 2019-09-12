module OpenGames.Examples.Source.Bimatrix where

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Preprocessor

-- Matching pennies with mixed Nash equilibrium, expressed as a Bayesian game

matchingPennies = Block [] []
                        [Line [] [] "reindex const (decision \"player1\" [Heads, Tails])" ["x"] ["matchingPenniesMatrix1 x y"],
                         Line [] [] "reindex const (decision \"player2\" [Heads, Tails])" ["y"] ["matchingPenniesMatrix2 x y"]]
                        [] []

meetingInNY = Block [] []
                        [Line [] [] "reindex const (decision \"player1\" [GCT, ES])" ["x"] ["meetingInNYMatrix x y"],
                         Line [] [] "reindex const (decision \"player2\" [GCT, ES])" ["y"] ["meetingInNYMatrix x y"]]
                        [] []


meetingInNY3 = Block [] []
                        [Line [] [] "reindex const (decision \"player1\" [GCT, ES])" ["x"] ["meetingInNYMatrix3 x y z"],
                         Line [] [] "reindex const (decision \"player2\" [GCT, ES])" ["y"] ["meetingInNYMatrix3 x y z"],
                         Line [] [] "reindex const (decision \"player3\" [GCT, ES])" ["z"] ["meetingInNYMatrix3 x y z"]]
                        [] []


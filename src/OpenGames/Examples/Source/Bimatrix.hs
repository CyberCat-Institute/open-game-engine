module OpenGames.Examples.Source.Bimatrix where

import OpenGames.Preprocessor.AbstractSyntax

-- Matching pennies with mixed Nash equilibrium, expressed as a Bayesian game

matchingPennies = Block [] []
                        [Line [] [] "reindex const (decision \"player1\" [Heads, Tails])" ["x"] ["matchingPenniesMatrix1 x y"],
                         Line [] [] "reindex const (decision \"player2\" [Heads, Tails])" ["y"] ["matchingPenniesMatrix2 x y"]]
                        [] []


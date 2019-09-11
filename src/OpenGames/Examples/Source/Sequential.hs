module OpenGames.Examples.Source.Sequential where

import OpenGames.Preprocessor.AbstractSyntax

sequential = Block [] []
                   [Line [] [] "reindex const (decision \"player1\" [GoLeft, GoRight])" ["x"] ["sequentialMatrix1 x y"],
                    Line ["x"] [] "decision \"player2\" [GoLeft, GoRight]" ["y"] ["sequentialMatrix2 x y"]]
                   [] []

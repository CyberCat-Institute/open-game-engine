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

-- Prisoner's dilemma

prisonersDilemma = Block [] []
                         [Line [] [] "reindex const (decision \"player1\" [Cooperate, Defect])" ["x"] ["prisonersDilemmaMatrix1 x y"],
                          Line [] [] "reindex const (decision \"player2\" [Cooperate, Defect])" ["y"] ["prisonersDilemmaMatrix2 x y"]]
                         [] []

-- Battle of the Sexes

bots = Block [] []
             [Line [] [] "reindex const (decision \"player1\" [Opera, Football])" ["x"] ["botsMatrix1 x y"],
              Line [] [] "reindex const (decision \"player2\" [Opera, Football])" ["y"] ["botsMatrix2 x y"]]
             [] []

-- Chicken Game

chicken = Block [] []
                [Line [] [] "reindex const (decision \"player1\" [Swerve, Straight])" ["x"] ["chickenMatrix1 x y"],
                 Line [] [] "reindex const (decision \"player2\" [Swerve, Straight])" ["y"] ["chickenMatrix2 x y"]]
                [] []

module OpenGames.Examples.Source.Bayesian where

import OpenGames.Preprocessor.AbstractSyntax

-- Bayesian prisoner's dilemma, from Mas-Colell, Whinston & Green p.254

bayesianPD = Block [] []
                   [Line [] [] "nature (uniform [Rat, Omerta])" ["t"] [],
                    Line [] [] "reindex const (decision \"prisoner1\" [Confess, DontConfess])" ["x"] ["pdMatrix1 x y"],
                    Line ["t"] [] "decision \"prisoner2\" [Confess, DontConfess]" ["y"] ["pdMatrix2 t x y"]]
                   [] []

bayesianBOS = Block [] []
                    [Line [] [] "nature (do {t1 <- uniform [BOSType1, BOSType2]; t2 <- uniform [BOSType1, BOSType2]; return (t1, t2)})" ["t1", "t2"] [],
                     Line ["t1"] [] "decision \"man\" [BayesianB, BayesianS]" ["x"] ["bos_bayesian_matrix1 t1 x y"],
                     Line ["t2"] [] "decision \"woman\" [BayesianB, BayesianS]" ["y"] ["bos_bayesian_matrix2 t2 x y"]]
                    [] []


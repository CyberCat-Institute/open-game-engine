module OpenGames.Examples.Source.SimpleAuction where

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Preprocessor

-- First-price sealed bid auction

firstPriceAuction = Block [] []
                           [Line [] [] "nature (uniform [0..6])" ["t1"] [],
                            Line [] [] "nature (uniform [0..6])" ["t2"] [],
                            Line ["t1"] [] "decision \"player1\" [0..12]" ["x"] ["playerOneUtility t1 x y"],
                            Line ["t2"] [] "decision \"player2\" [0..12]" ["y"] ["playerTwoUtility t2 x y"]]
                           [] []

-- Second-price sealed bid auction

secondPriceAuction = Block [] []
                           [Line [] [] "nature (uniform [0..6])" ["t1"] [],
                            Line [] [] "nature (uniform [0..6])" ["t2"] [],
                            Line ["t1"] [] "decision \"player1\" [0..12]" ["x"] ["playerOneUtility2nd t1 x y"],
                            Line ["t2"] [] "decision \"player2\" [0..12]" ["y"] ["playerTwoUtility2nd t2 x y"]]
                           [] []




-- First-price sealed bid auction with common valuation

firstPriceCommonValuation = Block [] []
                                  [Line [] [] "nature (uniform [10..16])" ["t"] [],
                                   Line [] [] "nature (uniform [10..16])" ["s1"] [],
                                   Line [] [] "nature (uniform [10..16])" ["s2"] [],
                                   Line [] [] "nature (uniform [False, True])" ["tieFavoursOne"] [],
                                   Line ["t + s1"] [] "decision \"player1\" [20..32]" ["x"] ["playerOneUtility_correlated t s1 s2 x y tieFavoursOne"],
                                   Line ["t + s2"] [] "decision \"player2\" [20..32]" ["y"] ["playerTwoUtility_correlated t s1 s2 x y tieFavoursOne"]]
                                  [] []

-- Discrete auction with correlated types, from https://www.ssc.wisc.edu/~dquint/econ899%20S2018/lecture%204.pdf

discreteAuction = Block [] []
                        [Line [] [] "nature discreteAuctionPrior" ["t1", "t2", "flip"] [],
                         Line ["t1"] [] "decision \"player1\" [Ten, Hundred]" ["x"] ["discreteAuctionUtility1 t1 x y flip"],
                         Line ["t2"] [] "decision \"player2\" [Ten, Hundred]" ["y"] ["discreteAuctionUtility2 t2 x y flip"]]
                        [] []


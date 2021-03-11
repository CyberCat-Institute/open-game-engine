{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module OpenGames.Examples.Bayesian where

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Compile
import Language.Haskell.TH.Syntax
import GHC.Generics

-- Bayesian prisoner's dilemma, from Mas-Colell, Whinston & Green p.254

data PDNature = Rat | Omerta deriving (Eq, Ord, Show)
data PDMove = Confess | DontConfess deriving (Eq, Ord, Show)

pdMatrix1 :: PDMove -> PDMove -> Rational
pdMatrix1 Confess Confess = -5
pdMatrix1 Confess DontConfess = -1
pdMatrix1 DontConfess Confess = -10
pdMatrix1 DontConfess DontConfess = 0

pdMatrix2 :: PDNature -> PDMove -> PDMove -> Rational
pdMatrix2 Rat Confess Confess = -5
pdMatrix2 Rat Confess DontConfess = -10
pdMatrix2 Rat DontConfess Confess = -1
pdMatrix2 Rat DontConfess DontConfess = -2
pdMatrix2 Omerta Confess Confess = -11
pdMatrix2 Omerta Confess DontConfess = -10
pdMatrix2 Omerta DontConfess Confess = -7
pdMatrix2 Omerta DontConfess DontConfess = -2

-- using TH
generateGame "bayesianPDTH" []
                   [Line [] [] [|nature (uniform [Rat, Omerta])|] ["t"] []
                   ,Line [] [] [|reindex const (decision "prisoner1" [Confess, DontConfess])|] ["x"] [[|pdMatrix1 x y|]]
                   ,Line [[|t|]] [] [|decision "prisoner2" [Confess, DontConfess]|] ["y"] [[|pdMatrix2 t x y|]]
                   ]

-- Using Quasiquotes
bayseianInline = [game|
  || =>>
    t | <- nature (uniform [Rat, Omerta])
        -< | ;
    x | pdMatrix1 x y
        <- reindex const (decision "prisoner1" [Confess, DontConfess])
        -< |;
    y | pdMatrix2 t x y
        <- decision "prisoner2" [Confess, DontConfess]
        -< | t ;
        <<= ||
  |]

-- Using TH
generateGame "bayesianPD" []
                   [Line [] [] [|nature (uniform [Rat, Omerta])|] ["t"] []
                   ,Line [] [] [|reindex const (decision "prisoner1" [Confess, DontConfess])|] ["x"] [[|pdMatrix1 x y|]]
                   ,Line [[|t|]] [] [|decision "prisoner2" [Confess, DontConfess]|] ["y"] [[|pdMatrix2 t x y|]]
                   ]


bayesianPDEquilibrium = equilibrium bayesianPD trivialContext

-----------------
-- Eq. strategies

{--
bayesianPDEquilibrium ((), certainly Confess, strategyPD2)

strategyPD2 :: PDNature -> OpenGames.Engine.BayesianOpenGames.D PDMove
strategyPD2 Rat =  certainly Confess
strategyPD2 Omerta = certainly DontConfess
--}


-- Battle of sexes with asymmetric uncertainty, from https://sites.duke.edu/niou/files/2011/05/Lecture-7-Bayesian-Games1.pdf

data BOSType = BOSType1 | BOSType2 deriving (Eq, Ord, Show, Generic)
data BOSMove = BayesianB | BayesianS deriving (Eq, Ord, Show, Generic)

bos_bayesian_matrix1, bos_bayesian_matrix2 :: BOSType -> BOSMove -> BOSMove -> Rational
bos_bayesian_matrix1 BOSType1 BayesianB BayesianB = 2
bos_bayesian_matrix1 BOSType1 BayesianS BayesianS = 1
bos_bayesian_matrix1 BOSType1 _ _ = 0
bos_bayesian_matrix1 BOSType2 BayesianB BayesianS = 2
bos_bayesian_matrix1 BOSType2 BayesianS BayesianB = 1
bos_bayesian_matrix1 BOSType2 _ _ = 0
bos_bayesian_matrix2 BOSType1 BayesianB BayesianB = 1
bos_bayesian_matrix2 BOSType1 BayesianS BayesianS = 2
bos_bayesian_matrix2 BOSType1 _ _ = 0
bos_bayesian_matrix2 BOSType2 BayesianS BayesianB = 1
bos_bayesian_matrix2 BOSType2 BayesianB BayesianS = 2
bos_bayesian_matrix2 BOSType2 _ _ = 0

bayesianBOS = [game|
  || =>>
    t1, t2 | <- nature (do {t1 <- uniform [BOSType1, BOSType2];
                            t2 <- uniform [BOSType1, BOSType2];
                            return (t1, t2)}) -< | ;
         x | bos_bayesian_matrix1 t1 x y <- decision "man" [BayesianB, BayesianS] -< | t1;
         y | bos_bayesian_matrix2 t2 x y <- decision "woman" [BayesianB, BayesianS] -< | t2;
    <<= ||
    |]
-- Using TH
generateGame "bayesianBOSTH" []
                    [Line []       [] [|nature (do {t1 <- uniform [BOSType1, BOSType2]; t2 <- uniform [BOSType1, BOSType2]; return (t1, t2)})|] ["t1", "t2"] []
                    ,Line [[|t1|]] [] [|decision "man" [BayesianB, BayesianS]|] ["x"] [[|bos_bayesian_matrix1 t1 x y|]]
                    ,Line [[|t2|]] [] [|decision "woman" [BayesianB, BayesianS]|] ["y"] [[|bos_bayesian_matrix2 t2 x y|]]
                    ]
bayesianBOSEquilibrium = equilibrium bayesianBOS trivialContext

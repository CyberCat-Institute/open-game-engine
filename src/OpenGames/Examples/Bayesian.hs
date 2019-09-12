module OpenGames.Examples.Bayesian where

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics

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

bayesianPD = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(t, x, y) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((t, x, y), ()) -> (t, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(Rat, 1), (Omerta, 1)]))))))) >>> (fromFunctions (\((), t) -> t) (\(t, x, y) -> ((t, x, y), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\t -> (t, ())) (\((t, x, y), ()) -> (t, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "prisoner1" [Confess, DontConfess]))))))) >>> (fromFunctions (\(t, x) -> (t, x)) (\(t, x, y) -> ((t, x, y), pdMatrix1 x y)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t, x) -> ((t, x), t)) (\((t, x, y), ()) -> (t, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "prisoner2" [Confess, DontConfess])))))) >>> (fromFunctions (\((t, x), y) -> (t, x, y)) (\(t, x, y) -> ((t, x, y), pdMatrix2 t x y))))))))) >>> (fromLens (\(t, x, y) -> ()) (curry (\((t, x, y), ()) -> (t, x, y)))))

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

data BOSType = BOSType1 | BOSType2 deriving (Eq, Ord, Show)
data BOSMove = BayesianB | BayesianS deriving (Eq, Ord, Show)

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

bayesianBOS = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(t1, t2, x, y) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (do {t1 <- uniform [BOSType1, BOSType2]; t2 <- uniform [BOSType1, BOSType2]; return (t1, t2)}))))))) >>> (fromFunctions (\((), (t1, t2)) -> (t1, t2)) (\(t1, t2, x, y) -> ((t1, t2, x, y), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2) -> ((t1, t2), t1)) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "man" [BayesianB, BayesianS])))))) >>> (fromFunctions (\((t1, t2), x) -> (t1, t2, x)) (\(t1, t2, x, y) -> ((t1, t2, x, y), bos_bayesian_matrix1 t1 x y)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2, x) -> ((t1, t2, x), t2)) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "woman" [BayesianB, BayesianS])))))) >>> (fromFunctions (\((t1, t2, x), y) -> (t1, t2, x, y)) (\(t1, t2, x, y) -> ((t1, t2, x, y), bos_bayesian_matrix2 t2 x y))))))))) >>> (fromLens (\(t1, t2, x, y) -> ()) (curry (\((t1, t2, x, y), ()) -> (t1, t2, x, y)))))

bayesianBOSEquilibrium = equilibrium bayesianBOS trivialContext

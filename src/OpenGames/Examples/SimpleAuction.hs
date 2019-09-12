module OpenGames.Examples.SimpleAuction where

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics

-- Second-price sealed bid auction

type SimpleAuctionType = Rational
type SimpleAuctionBid = Rational

playerOneWins :: SimpleAuctionBid -> SimpleAuctionBid -> Bool
playerOneWins x y = x > y

playerOnePays, playerTwoPays :: SimpleAuctionBid -> SimpleAuctionBid -> Rational
playerOnePays x y = if playerOneWins x y then x else 0
playerTwoPays x y = if playerOneWins x y then 0 else y

playerOneUtility, playerTwoUtility :: SimpleAuctionType -> SimpleAuctionBid -> SimpleAuctionBid -> Rational
playerOneUtility t1 x y = if playerOneWins x y then t1 - playerOnePays x y else 0
playerTwoUtility t2 x y = if playerOneWins x y then 0 else t2 - playerTwoPays x y

firstPriceAuction = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(t1, t2, x, y) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0..6]))))))) >>> (fromFunctions (\((), t1) -> t1) (\(t1, t2, x, y) -> ((t1, t2, x, y), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\t1 -> (t1, ())) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0..6]))))))) >>> (fromFunctions (\(t1, t2) -> (t1, t2)) (\(t1, t2, x, y) -> ((t1, t2, x, y), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2) -> ((t1, t2), t1)) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [0..12])))))) >>> (fromFunctions (\((t1, t2), x) -> (t1, t2, x)) (\(t1, t2, x, y) -> ((t1, t2, x, y), playerOneUtility t1 x y)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2, x) -> ((t1, t2, x), t2)) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [0..12])))))) >>> (fromFunctions (\((t1, t2, x), y) -> (t1, t2, x, y)) (\(t1, t2, x, y) -> ((t1, t2, x, y), playerTwoUtility t2 x y))))))))) >>> (fromLens (\(t1, t2, x, y) -> ()) (curry (\((t1, t2, x, y), ()) -> (t1, t2, x, y)))))

firstPriceAuctionEquilibrium = equilibrium firstPriceAuction trivialContext



-----------------
-- Eq. strategies

strategyAuction :: Num prob => SimpleAuctionType -> T prob SimpleAuctionBid
strategyAuction t = certainly $ t/2

firstPriceAuctionEquilibrium ((),(), strategyFPAuction, strategyFPAuction)
--}


-- Second-price sealed bid auction

playerOnePays2nd, playerTwoPays2nd :: SimpleAuctionBid -> SimpleAuctionBid -> Rational
playerOnePays2nd x y = if playerOneWins x y then y else 0
playerTwoPays2nd x y = if playerOneWins x y then 0 else x

playerOneUtility2nd, playerTwoUtility2nd :: SimpleAuctionType -> SimpleAuctionBid -> SimpleAuctionBid -> Rational
playerOneUtility2nd t1 x y = if playerOneWins x y then t1 - playerOnePays2nd x y else 0
playerTwoUtility2nd t2 x y = if playerOneWins x y then 0 else t2 - playerTwoPays2nd x y


secondPriceAuction = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(t1, t2, x, y) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0..6]))))))) >>> (fromFunctions (\((), t1) -> t1) (\(t1, t2, x, y) -> ((t1, t2, x, y), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\t1 -> (t1, ())) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0..6]))))))) >>> (fromFunctions (\(t1, t2) -> (t1, t2)) (\(t1, t2, x, y) -> ((t1, t2, x, y), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2) -> ((t1, t2), t1)) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [0..12])))))) >>> (fromFunctions (\((t1, t2), x) -> (t1, t2, x)) (\(t1, t2, x, y) -> ((t1, t2, x, y), playerOneUtility2nd t1 x y)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2, x) -> ((t1, t2, x), t2)) (\((t1, t2, x, y), ()) -> (t1, t2, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [0..12])))))) >>> (fromFunctions (\((t1, t2, x), y) -> (t1, t2, x, y)) (\(t1, t2, x, y) -> ((t1, t2, x, y), playerTwoUtility2nd t2 x y))))))))) >>> (fromLens (\(t1, t2, x, y) -> ()) (curry (\((t1, t2, x, y), ()) -> (t1, t2, x, y)))))



secondPriceAuctionEquilibrium = equilibrium secondPriceAuction trivialContext

-----------------
-- Eq. strategies

{--
strategyAuction' :: Num prob => SimpleAuctionType -> T prob SimpleAuctionBid
strategyAuction' t = certainly t

secondPriceAuctionEquilibrium ((),(), strategyAuction', strategyAuction')
--}



-- First-price sealed bid auction with common valuation

type SimpleAuctionPrivateType = Rational

playerOneUtility_correlated, playerTwoUtility_correlated :: SimpleAuctionType -> SimpleAuctionPrivateType -> SimpleAuctionPrivateType -> SimpleAuctionBid -> SimpleAuctionBid -> Bool -> Rational
playerOneUtility_correlated t s1 s2 x y tieFavoursOne | (x > y || (x == y && tieFavoursOne)) = (2*t + s1 + s2)/2 - x
                                                      | (x < y || (x == y && not tieFavoursOne)) = 0
playerTwoUtility_correlated t s1 s2 x y tieFavoursOne | (x > y || (x == y && tieFavoursOne)) = 0
                                                      | (x < y || (x == y && not tieFavoursOne)) = (2*t + s1 + s2)/2 - y

firstPriceCommonValuation = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(t, s1, s2, tieFavoursOne, x, y) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5, a6) -> (((((a1, a2), a3), a4), a5), a6)) ((((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [10..16]))))))) >>> (fromFunctions (\((), t) -> t) (\(t, s1, s2, tieFavoursOne, x, y) -> ((t, s1, s2, tieFavoursOne, x, y), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\t -> (t, ())) (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [10..16]))))))) >>> (fromFunctions (\(t, s1) -> (t, s1)) (\(t, s1, s2, tieFavoursOne, x, y) -> ((t, s1, s2, tieFavoursOne, x, y), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t, s1) -> ((t, s1), ())) (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [10..16]))))))) >>> (fromFunctions (\((t, s1), s2) -> (t, s1, s2)) (\(t, s1, s2, tieFavoursOne, x, y) -> ((t, s1, s2, tieFavoursOne, x, y), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t, s1, s2) -> ((t, s1, s2), ())) (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [False, True]))))))) >>> (fromFunctions (\((t, s1, s2), tieFavoursOne) -> (t, s1, s2, tieFavoursOne)) (\(t, s1, s2, tieFavoursOne, x, y) -> ((t, s1, s2, tieFavoursOne, x, y), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t, s1, s2, tieFavoursOne) -> ((t, s1, s2, tieFavoursOne), t + s1)) (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [20..32])))))) >>> (fromFunctions (\((t, s1, s2, tieFavoursOne), x) -> (t, s1, s2, tieFavoursOne, x)) (\(t, s1, s2, tieFavoursOne, x, y) -> ((t, s1, s2, tieFavoursOne, x, y), playerOneUtility_correlated t s1 s2 x y tieFavoursOne)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t, s1, s2, tieFavoursOne, x) -> ((t, s1, s2, tieFavoursOne, x), t + s2)) (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [20..32])))))) >>> (fromFunctions (\((t, s1, s2, tieFavoursOne, x), y) -> (t, s1, s2, tieFavoursOne, x, y)) (\(t, s1, s2, tieFavoursOne, x, y) -> ((t, s1, s2, tieFavoursOne, x, y), playerTwoUtility_correlated t s1 s2 x y tieFavoursOne))))))))) >>> (fromLens (\(t, s1, s2, tieFavoursOne, x, y) -> ()) (curry (\((t, s1, s2, tieFavoursOne, x, y), ()) -> (t, s1, s2, tieFavoursOne, x, y)))))

firstPriceCommonValuationEquilibrium = equilibrium firstPriceCommonValuation trivialContext

-- Discrete auction with correlated types, from https://www.ssc.wisc.edu/~dquint/econ899%20S2018/lecture%204.pdf

data DiscreteAuctionType = Ten | Hundred deriving (Eq, Ord, Show)
type DiscreteAuctionBid = DiscreteAuctionType

discreteAuctionBidValue :: DiscreteAuctionBid -> Rational
discreteAuctionBidValue Ten = 10
discreteAuctionBidValue Hundred = 100

discreteAuctionPrior :: T Rational (DiscreteAuctionType, DiscreteAuctionType, Bool)
discreteAuctionPrior = do (t1, t2) <- fromFreqs [((Ten, Ten), 2), ((Hundred, Hundred), 2), ((Ten, Hundred), 1), ((Hundred, Ten), 1)]
                          flip <- uniform [True, False]
                          return (t1, t2, flip)

discreteAuctionUtility1, discreteAuctionUtility2 :: DiscreteAuctionType -> DiscreteAuctionBid -> DiscreteAuctionBid -> Bool -> Rational
discreteAuctionUtility1 t Hundred Hundred True = discreteAuctionBidValue t - 100
discreteAuctionUtility1 t Hundred Hundred False = 0
discreteAuctionUtility1 t Hundred Ten _ = discreteAuctionBidValue t - 100
discreteAuctionUtility1 t Ten Hundred _ = -30
discreteAuctionUtility1 t Ten Ten True = 15
discreteAuctionUtility1 t Ten Ten False = 5 + discreteAuctionBidValue t
discreteAuctionUtility2 t Hundred Hundred True = 0
discreteAuctionUtility2 t Hundred Hundred False = discreteAuctionBidValue t - 100
discreteAuctionUtility2 t Hundred Ten _ = -30
discreteAuctionUtility2 t Ten Hundred _ = discreteAuctionBidValue t - 100
discreteAuctionUtility2 t Ten Ten True = 5 + discreteAuctionBidValue t
discreteAuctionUtility2 t Ten Ten False = 15

discreteAuction = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(t1, t2, flip, x, y) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((t1, t2, flip, x, y), ()) -> (t1, t2, flip, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature discreteAuctionPrior)))))) >>> (fromFunctions (\((), (t1, t2, flip)) -> (t1, t2, flip)) (\(t1, t2, flip, x, y) -> ((t1, t2, flip, x, y), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2, flip) -> ((t1, t2, flip), t1)) (\((t1, t2, flip, x, y), ()) -> (t1, t2, flip, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [Ten, Hundred])))))) >>> (fromFunctions (\((t1, t2, flip), x) -> (t1, t2, flip, x)) (\(t1, t2, flip, x, y) -> ((t1, t2, flip, x, y), discreteAuctionUtility1 t1 x y flip)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(t1, t2, flip, x) -> ((t1, t2, flip, x), t2)) (\((t1, t2, flip, x, y), ()) -> (t1, t2, flip, x, y))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [Ten, Hundred])))))) >>> (fromFunctions (\((t1, t2, flip, x), y) -> (t1, t2, flip, x, y)) (\(t1, t2, flip, x, y) -> ((t1, t2, flip, x, y), discreteAuctionUtility2 t2 x y flip))))))))) >>> (fromLens (\(t1, t2, flip, x, y) -> ()) (curry (\((t1, t2, flip, x, y), ()) -> (t1, t2, flip, x, y)))))

discreteAuctionEquilibrium = equilibrium discreteAuction trivialContext

module OpenGames.Examples.LemonMarket where

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics

data LemonQuality = Good | Bad deriving (Eq, Ord, Show)
data LemonPrice = Low | High deriving (Eq, Ord, Show)
data LemonBuy = Buy | NotBuy deriving (Eq, Ord, Show)

lemonValuationSeller, lemonValuationBuyer :: LemonQuality -> Rational
lemonValuationSeller Good = 100
lemonValuationSeller Bad = 50
lemonValuationBuyer Good = 120
lemonValuationBuyer Bad = 70

lemonPrice :: LemonPrice -> Rational
lemonPrice Low = 60
lemonPrice High = 110

lemonUtilitySeller, lemonUtilityBuyer :: LemonQuality -> LemonPrice -> LemonBuy -> Rational
lemonUtilitySeller quality price Buy = lemonPrice price - lemonValuationSeller quality
lemonUtilitySeller quality price NotBuy = 0
lemonUtilityBuyer quality price Buy = lemonValuationBuyer quality - lemonPrice price
lemonUtilityBuyer quality price NotBuy = 0

lemonMarket = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(quality, price, buy) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((quality, price, buy), ()) -> (quality, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(Good, 1), (Bad, 4)]))))))) >>> (fromFunctions (\((), quality) -> quality) (\(quality, price, buy) -> ((quality, price, buy), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\quality -> (quality, quality)) (\((quality, price, buy), ()) -> (quality, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "seller" [Low, High])))))) >>> (fromFunctions (\(quality, price) -> (quality, price)) (\(quality, price, buy) -> ((quality, price, buy), lemonUtilitySeller quality price buy)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(quality, price) -> ((quality, price), price)) (\((quality, price, buy), ()) -> (quality, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "buyer" [Buy, NotBuy])))))) >>> (fromFunctions (\((quality, price), buy) -> (quality, price, buy)) (\(quality, price, buy) -> ((quality, price, buy), lemonUtilityBuyer quality price buy))))))))) >>> (fromLens (\(quality, price, buy) -> ()) (curry (\((quality, price, buy), ()) -> (quality, price, buy)))))

lemonMarketEquilibrium = equilibrium lemonMarket trivialContext

strategySeller :: Num prob => LemonQuality -> T prob LemonPrice 
strategySeller = (\ _ -> certainly $ High)

strategyBuyer :: Num prob => LemonPrice -> T prob LemonBuy
strategyBuyer = (\ _ -> certainly $ NotBuy)
-- Generated from the Statebox KD compiler

lemonMarketKD = let n = nature (fromFreqs [(Good, 1), (Bad, 4)])
                    x = fromFunctions (\x -> (x, x)) id
                    y = fromFunctions (\x -> (x, x)) id
                    s = decision "seller" [Low, High]
                    b = decision "buyer" [Buy, NotBuy]
                    u = counitFunction (\(quality, price, buy) -> (lemonUtilityBuyer quality price buy, lemonUtilitySeller quality price buy))
  in (n >>> x >>> (fromFunctions (\(q_0, q0_1) -> (q_0, q0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& (s >>> (fromFunctions (\p0_0 -> (p0_0, ())) (\((), r_1) -> r_1) >>> ((y >>> (fromFunctions (\(p_0, p0_1) -> (p_0, p0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& b) >>> fromFunctions (\(p_0, b0_1) -> (p_0, b0_1)) (\r1_1 -> ((), r1_1)))) &&& fromFunctions id id) >>> fromFunctions (\((p_0_0, b0_1_0), ()) -> (p_0_0, b0_1_0)) (\(r1_1_0, r_1) -> (r1_1_0, r_1))))) >>> fromFunctions (\(q_0, (p_0_0_1, b0_1_0_1)) -> (q_0, p_0_0_1, b0_1_0_1)) (\(r1_1_0_1, r_1_1) -> ((), (r1_1_0_1, r_1_1)))) >>> u) 


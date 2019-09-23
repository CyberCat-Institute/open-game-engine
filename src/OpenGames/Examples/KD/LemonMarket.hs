module OpenGames.Examples.KD.LemonMarket where

-- Lemon market generated from KD editor
-- https://kdmoncatog.glitch.me/#pixels=NxQQQU%0ANxSyPU%0ANxSyBU%0ANxSrrU&context=N:%20-%3E%20Q%0Ax:%20Q%20-%3E%20Q%20Q%0Ay:%20P%20-%3E%20P%20P%0AS:%20Q%20-%3E%20P%20r%0AB:%20P%20-%3E%20B%20r%0AU:%20Q%20P%20B%20r%20r%20-%3E%0A

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnosticsTLL
import OpenGames.Engine.BayesianDiagnostics (trivialContext)

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

lemonMarketKD good bad = let n = nature (fromFreqs [(Good, good), (Bad, bad)])
                             x = fromFunctions (\x -> (x, x)) id
                             y = fromFunctions (\x -> (x, x)) id
                             s = decision "seller" [Low, High]
                             b = decision "buyer" [Buy, NotBuy]
                             u = counitFunction (\(quality, price, buy) -> (lemonUtilityBuyer quality price buy, lemonUtilitySeller quality price buy))
  in (n >>> x >>> (fromFunctions (\(q_0, q0_1) -> (q_0, q0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& (s >>> (fromFunctions (\p0_0 -> (p0_0, ())) (\((), r_1) -> r_1) >>> ((y >>> (fromFunctions (\(p_0, p0_1) -> (p_0, p0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& b) >>> fromFunctions (\(p_0, b0_1) -> (p_0, b0_1)) (\r1_1 -> ((), r1_1)))) &&& fromFunctions id id) >>> fromFunctions (\((p_0_0, b0_1_0), ()) -> (p_0_0, b0_1_0)) (\(r1_1_0, r_1) -> (r1_1_0, r_1))))) >>> fromFunctions (\(q_0, (p_0_0_1, b0_1_0_1)) -> (q_0, p_0_0_1, b0_1_0_1)) (\(r1_1_0_1, r_1_1) -> ((), (r1_1_0_1, r_1_1)))) >>> u) 

lemonMarketKDEquilibrium = equilibrium (lemonMarketKD 11 1) trivialContext

-- Eq. strategies for eq. distribution fromFrequs [(Good, 1), (Bad, 4)]
strategySeller :: Num prob => LemonQuality -> T prob LemonPrice 
strategySeller = (\ _ -> certainly $ High)

strategyBuyer :: Num prob => LemonPrice -> T prob LemonBuy
strategyBuyer = (\ _ -> certainly $ NotBuy)

-- Eq. strategies for eq. distribution fromFrequs [(Good, 11), (Bad, 1)]
strategySeller' :: Num prob => LemonQuality -> T prob LemonPrice 
strategySeller' = (\ _ -> certainly $ High)

strategyBuyer' :: Num prob => LemonPrice -> T prob LemonBuy
strategyBuyer' = (\ _ -> certainly $ Buy)

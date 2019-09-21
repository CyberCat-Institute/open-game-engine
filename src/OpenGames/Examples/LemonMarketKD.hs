module OpenGames.Examples.LemonMarketKD where

-- Lemon market generated from KD editor

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnosticsTLL
import OpenGames.Examples.LemonMarket

lemonMarketKD = let n = nature (fromFreqs [(Good, 1), (Bad, 4)])
                    x = fromFunctions (\x -> (x, x)) id
                    y = fromFunctions (\x -> (x, x)) id
                    s = decision "seller" [Low, High]
                    b = decision "buyer" [Buy, NotBuy]
                    u = counitFunction (\(quality, price, buy) -> (lemonUtilityBuyer quality price buy, lemonUtilitySeller quality price buy))
  in (n >>> x >>> (fromFunctions (\(q_0, q0_1) -> (q_0, q0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& (s >>> (fromFunctions (\p0_0 -> (p0_0, ())) (\((), r_1) -> r_1) >>> ((y >>> (fromFunctions (\(p_0, p0_1) -> (p_0, p0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& b) >>> fromFunctions (\(p_0, b0_1) -> (p_0, b0_1)) (\r1_1 -> ((), r1_1)))) &&& fromFunctions id id) >>> fromFunctions (\((p_0_0, b0_1_0), ()) -> (p_0_0, b0_1_0)) (\(r1_1_0, r_1) -> (r1_1_0, r_1))))) >>> fromFunctions (\(q_0, (p_0_0_1, b0_1_0_1)) -> (q_0, p_0_0_1, b0_1_0_1)) (\(r1_1_0_1, r_1_1) -> ((), (r1_1_0_1, r_1_1)))) >>> u) 


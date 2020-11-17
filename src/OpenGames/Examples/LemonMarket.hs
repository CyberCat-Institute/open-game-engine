{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module OpenGames.Examples.LemonMarket where

import GHC.Generics
import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax

data LemonQuality = Good | Bad deriving (Eq, Ord, Show, Generic, Enum)
data LemonPrice = Low | High deriving (Eq, Ord, Show, Generic, Enum)
data LemonBuy = Buy | NotBuy deriving (Eq, Ord, Show, Generic, Enum)

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

-- Using TH
generateGame "lemonMarketTH" [] [QLine [] [] [|nature (fromFreqs [(Good, 1), (Bad, 4)])|] ["quality"] []
                                ,QLine [param "quality"] [] [|decision "seller" [Low, High] |] ["price"] [[|lemonUtilitySeller quality price buy|]]
                                ,QLine [param "price"] [] [|decision "buyer" [Buy, NotBuy]|] ["buy"] [[|lemonUtilityBuyer quality price buy|]]]

-- Using Blocks
lemonMarketSrc = Block [] []
                    [Line [] [] "nature (fromFreqs [(Good, 1), (Bad, 4)])" ["quality"] [],
                     Line ["quality"] [] "decision \"seller\" [Low, High]" ["price"] ["lemonUtilitySeller quality price buy"],
                     Line ["price"] [] "decision \"buyer\" [Buy, NotBuy]" ["buy"] ["lemonUtilityBuyer quality price buy"]]
                    [] []

lemonMarket = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(quality, price, buy) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((quality, price, buy), ()) -> (quality, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(Good, 1), (Bad, 4)]))))))) >>> (fromFunctions (\((), quality) -> quality) (\(quality, price, buy) -> ((quality, price, buy), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\quality -> (quality, quality)) (\((quality, price, buy), ()) -> (quality, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "seller" [Low, High])))))) >>> (fromFunctions (\(quality, price) -> (quality, price)) (\(quality, price, buy) -> ((quality, price, buy), lemonUtilitySeller quality price buy)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(quality, price) -> ((quality, price), price)) (\((quality, price, buy), ()) -> (quality, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "buyer" [Buy, NotBuy])))))) >>> (fromFunctions (\((quality, price), buy) -> (quality, price, buy)) (\(quality, price, buy) -> ((quality, price, buy), lemonUtilityBuyer quality price buy))))))))) >>> (fromLens (\(quality, price, buy) -> ()) (curry (\((quality, price, buy), ()) -> (quality, price, buy)))))

lemonMarketEquilibrium = equilibrium lemonMarket trivialContext



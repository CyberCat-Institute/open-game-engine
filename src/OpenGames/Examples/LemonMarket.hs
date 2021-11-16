{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric #-}
module OpenGames.Examples.LemonMarket where

import GHC.Generics
import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.Compile
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
generateGame "lemonMarketTH" [] [mkLine [] [] [|nature (fromFreqs [(Good, 1), (Bad, 4)])|] ["quality"] []
                                ,mkLine [param "quality"] [] [|decision "seller" [Low, High] |] ["price"] [[|lemonUtilitySeller quality price buy|]]
                                ,mkLine [param "price"] [] [|decision "buyer" [Buy, NotBuy]|] ["buy"] [[|lemonUtilityBuyer quality price buy|]]]

-- Using QuasiQuotes
lemonMarket = [game|
  || =>>
  quality | <- nature (fromFreqs [(Good, 1), (Bad, 4)]) -< | ;
  price   | lemonUtilitySeller quality price buy <- decision "seller" [Low, High] -< | quality ;
  buy     | lemonUtilityBuyer quality price buy <- decision "buyer" [Buy, NotBuy] -< | price ;
  <<= ||
|]

lemonMarketEquilibrium = equilibrium lemonMarket trivialContext



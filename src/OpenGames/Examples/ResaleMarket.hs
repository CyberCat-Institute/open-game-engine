{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module OpenGames.Examples.ResaleMarket where

import GHC.Generics
import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax

-- Public good resale market

data SellerType = LowSellerType | HighSellerType deriving (Eq, Ord, Show, Generic)
data BuyerType = LowBuyerType | HighBuyerType deriving (Eq, Ord, Show)
data Price = LowPrice | MediumPrice | HighPrice deriving (Eq, Ord, Show, Generic)
data BuyGood = BuyGood | NotBuyGood deriving (Eq, Ord, Show)

price :: Price -> Rational
price LowPrice = 0
price MediumPrice = 7
price HighPrice = 15

buyerValuation :: BuyerType -> Rational
buyerValuation LowBuyerType = 0
buyerValuation HighBuyerType = 15

sellerValuation :: SellerType -> Rational
sellerValuation LowSellerType = 0
sellerValuation HighSellerType = 15

sellerUtility :: SellerType -> Price -> BuyGood -> Rational
sellerUtility t _ NotBuyGood = sellerValuation t
sellerUtility _ p BuyGood = price p

buyerUtility :: BuyerType -> Price -> BuyGood -> Rational
buyerUtility _ _ NotBuyGood = 0
buyerUtility t p BuyGood = buyerValuation t - price p

pgResale_prior :: T Rational (SellerType, BuyerType)
pgResale_prior = fromFreqs [((LowSellerType, LowBuyerType), 2), ((HighSellerType, HighBuyerType), 2), ((LowSellerType, HighBuyerType), 1), ((HighSellerType, LowBuyerType), 1)]

-- Using TH
generateGame "resaleMarketTH" [] $
                     [line []                   [] [|nature pgResale_prior|]                                ["sellerType", "buyerType"] [],
                      line [param "sellerType"] [] [|decision "seller" [LowPrice, MediumPrice, HighPrice]|] ["price'"]                   [[|sellerUtility sellerType price' buy|]],
                      line [param "price'"]      [] [|decision "buyer" [BuyGood, NotBuyGood]|]               ["buy"]                     [[|buyerUtility buyerType price' buy|]]]

-- Using Blocks
resaleMarketSrc = Block [] []
                     [Line [] [] "nature pgResale_prior" ["sellerType", "buyerType"] [],
                      Line ["sellerType"] [] "decision \"seller\" [LowPrice, MediumPrice, HighPrice]" ["price"] ["sellerUtility sellerType price buy"],
                      Line ["price"] [] "decision \"buyer\" [BuyGood, NotBuyGood]" ["buy"] ["buyerUtility buyerType price buy"]]
                     [] []

resaleMarket = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(sellerType, buyerType, price, buy) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((sellerType, buyerType, price, buy), ()) -> (sellerType, buyerType, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature pgResale_prior)))))) >>> (fromFunctions (\((), (sellerType, buyerType)) -> (sellerType, buyerType)) (\(sellerType, buyerType, price, buy) -> ((sellerType, buyerType, price, buy), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sellerType, buyerType) -> ((sellerType, buyerType), sellerType)) (\((sellerType, buyerType, price, buy), ()) -> (sellerType, buyerType, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "seller" [LowPrice, MediumPrice, HighPrice])))))) >>> (fromFunctions (\((sellerType, buyerType), price) -> (sellerType, buyerType, price)) (\(sellerType, buyerType, price, buy) -> ((sellerType, buyerType, price, buy), sellerUtility sellerType price buy)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sellerType, buyerType, price) -> ((sellerType, buyerType, price), price)) (\((sellerType, buyerType, price, buy), ()) -> (sellerType, buyerType, price, buy))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "buyer" [BuyGood, NotBuyGood])))))) >>> (fromFunctions (\((sellerType, buyerType, price), buy) -> (sellerType, buyerType, price, buy)) (\(sellerType, buyerType, price, buy) -> ((sellerType, buyerType, price, buy), buyerUtility buyerType price buy))))))))) >>> (fromLens (\(sellerType, buyerType, price, buy) -> ()) (curry (\((sellerType, buyerType, price, buy), ()) -> (sellerType, buyerType, price, buy)))))

resaleMarketEquilibrium = equilibrium resaleMarket trivialContext

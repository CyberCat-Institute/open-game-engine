{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module THTests where

import CensorTests
import GHC.Real
import Data.Bool
import Data.List
import Test.Hspec
import Test.QuickCheck
import Language.Haskell.TH.Syntax

import OpenGames.Preprocessor.THSyntax as THS
import OpenGames.Preprocessor.Preprocessor as Pre
import OpenGames.Preprocessor.TH
import OpenGames.Preprocessor.AbstractSyntax as ABS

import OpenGames.Examples.SimpleAuction
import OpenGames.Examples.Bimatrix
import OpenGames.Examples.Bayesian as B
import OpenGames.Examples.LemonMarket
import OpenGames.Examples.ResaleMarket
import OpenGames.Examples.RepetitionTest
import OpenGames.Examples.Sequential
import OpenGames.Examples.Signalling

import Numeric.Probability.Distribution
import OpenGames.Engine.BayesianDiagnostics

instance Arbitrary Coordination where
  arbitrary = bool GCT ES <$> arbitrary
instance Arbitrary Coin where
  arbitrary = bool Heads Tails <$> arbitrary
instance Arbitrary BOSMove where
  arbitrary = bool BayesianB BayesianS <$> arbitrary
instance Arbitrary LemonPrice where
  arbitrary = bool Low High <$> arbitrary
instance Arbitrary LemonBuy where
  arbitrary = bool Buy NotBuy <$> arbitrary
instance Arbitrary Price where
  arbitrary = elements [LowPrice, MediumPrice, HighPrice]
instance Arbitrary BuyGood where
  arbitrary = elements [BuyGood, NotBuyGood]
instance Arbitrary SequentialMove where
  arbitrary = bool GoLeft GoRight <$> arbitrary
instance Arbitrary Effort where
  arbitrary = bool LowEffort HighEffort <$> arbitrary
instance Arbitrary Wage where
  arbitrary = bool LowWage HighWage <$> arbitrary
instance Arbitrary Contract where
  arbitrary = bool Accept NotAccept <$> arbitrary
instance Arbitrary DiscreteAuctionType where
  arbitrary = bool Ten Hundred <$> arbitrary

instance CoArbitrary BOSMove where
instance CoArbitrary LemonQuality where
instance CoArbitrary LemonPrice where
instance CoArbitrary BOSType where
instance CoArbitrary SellerType where
instance CoArbitrary Price where
instance CoArbitrary SequentialMove where
instance CoArbitrary Productivity where
instance CoArbitrary Effort where
instance CoArbitrary Wage where
instance CoArbitrary DiscreteAuctionType where

instance Function BOSType where
instance Function LemonPrice where
instance Function LemonBuy where
instance Function LemonQuality where
instance Function SellerType where
instance Function Price where
instance Function SequentialMove where
instance Function Productivity where
instance Function Effort where
instance Function Wage where
instance Function DiscreteAuctionType where

instance Arbitrary t => Arbitrary (T Rational t) where
  arbitrary = certainly <$> arbitrary
  shrink (Cons v) = Cons <$> (subsequences v)

main :: IO ()
main = do
  testCensor
  hspec $ parallel $ do
    describe "testing regressions with old syntax" $ parallel $ do

      it "should find the same equilibrium with firstPriceAuction" $ property $
        \(Fn (f :: Ratio Integer -> T Rational (Ratio Integer)))
         (Fn (g :: Ratio Integer -> T Rational (Ratio Integer)))
          -> firstPriceAuctionEquilibrium ((), (), f, g)
          == equilibrium firstPriceAuctionTH trivialContext ((), (), f, g)

      it "should find the same equilibrium with secondPriceAuction" $ property $
        \(Fn (f :: Ratio Integer -> T Rational (Ratio Integer)))
         (Fn (g :: Ratio Integer -> T Rational (Ratio Integer)))
                -> equilibrium secondPriceAuctionTH trivialContext((), (), f, g)
                == equilibrium secondPriceAuction trivialContext ((), (), f, g)

      it "should find the same equilibrium with pennies" $ property $
        \x -> equilibrium matchingPennies trivialContext x
           == equilibrium matchingPenniesTH trivialContext x

      it "should find the same equilibrium with bayesian" $ property $
        \(Fn (f :: BOSType -> T Rational BOSMove))
         (Fn (g :: BOSType -> T Rational BOSMove))
                -> equilibrium bayesianBOS trivialContext ((), f, g)
                == equilibrium bayesianBOSTH trivialContext ((), f, g)

      it "should find the same equilibrium with meetingInNY" $ property $
        \x -> equilibrium meetingInNY trivialContext x
           == equilibrium meetingInNYTH trivialContext x

      it "should find the same equilibrium with meetingInNY3" $ property $
        \x -> equilibrium meetingInNY3 trivialContext x
           == equilibrium meetingInNY3TH trivialContext x

      it "should find the same equilibrium with q lemons" $ property $
        \(Fn (sell :: LemonQuality -> T Rational LemonPrice))
         (Fn (buy :: LemonPrice -> T Rational LemonBuy))
          -> lemonMarketEquilibrium ((), sell, buy)
          == equilibrium lemonMarketTH trivialContext ((), sell, buy)

      it "should find the same equilibrium with resale market " $ property $
        \(Fn (sell :: SellerType -> T Rational Price))
         (Fn (buy :: Price -> T Rational BuyGood))
          -> resaleMarketEquilibrium ((), sell, buy)
          == equilibrium resaleMarketTH trivialContext ((), sell, buy)

      it "should find the same equilibrium with sequential" $ property $
        \(Fn (seq :: SequentialMove -> T Rational SequentialMove))
         x
          -> sequentialEquilibrium (x, seq)
          == equilibrium sequentialTH trivialContext (x, seq)

      it "should find the same equilibrium with Signalling" $ property $
        \(Fn (effort :: Productivity -> T Rational Effort))
         (Fn (wage :: Effort -> T Rational Wage))
         (Fn (decision :: (Productivity, Wage) -> T Rational Contract))
          -> equilibrium signalling trivialContext ((), effort, wage, decision)
          == equilibrium signallingTH trivialContext ((), effort, wage, decision)
      it "should find the same equilibrium with discrete auction" $ property $
        \(Fn (x :: DiscreteAuctionType -> T Rational DiscreteAuctionType))
         (Fn (y :: DiscreteAuctionType -> T Rational DiscreteAuctionType))
           -> equilibrium discreteAuction trivialContext ((), x, y)
           == equilibrium discreteAuctionTH trivialContext ((), x, y)




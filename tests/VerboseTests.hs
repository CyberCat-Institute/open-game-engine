{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VerboseTests where

import GHC.Real
import Data.Bool
import Data.List
import Data.Bifunctor
import Data.Coerce
import Test.Hspec
import Test.QuickCheck

import OpenGames.Preprocessor.THSyntax as THS
import OpenGames.Preprocessor.Preprocessor as Pre
import OpenGames.Preprocessor.TH
import OpenGames.Preprocessor.Compile
import OpenGames.Preprocessor.AbstractSyntax as ABS

import Control.Arrow (Kleisli(..))
import Numeric.Probability.Distribution (certainly, uniform, fromFreqs, T(..), equal)

import Language.Haskell.TH

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.DependentDecision
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)

import OpenGames.Examples.Consensus.DepositGame

newtype IsEq a = UnEq { pair :: (a, a) }

assertEq :: a -> a -> IsEq a
assertEq x y = UnEq (x, y)

instance (Ord a, Arbitrary s, Show s) => Testable (IsEq (StochasticStatefulOptic s t a b)) where
  property (UnEq (StochasticStatefulOptic f g, StochasticStatefulOptic f' g')) =
    forAll ((,) <$> arbitrary <*> arbitrary) (\(x, y) -> fmap snd (f x) `equal` fmap snd (f' y))

instance (Arbitrary a, Arbitrary b) => (Arbitrary (T a b)) where
  arbitrary = Cons <$> arbitrary
  shrink (Cons ls) = Cons <$> (shrink ls)

-- instance Arbitrary a => (Arbitrary (StochasticStatefulContext a () Double ())) where
--   arbitrary = StochasticStatefulContext @() <$> arbitrary <*> pure (const (const (pure ())))
--   shrink (StochasticStatefulContext st fn) =
--     [StochasticStatefulContext @() st' (const (const (pure ()))) | st' <- shrink (fmap (\(_,x) -> ((), x)) st)]

instance (Arbitrary (Kleisli Stochastic Double Double)) where
  arbitrary = let v = arbitrary :: Gen (T (Positive Double) (Positive Double)) in Kleisli <$>  fmap const (coerce v)
  shrink (Kleisli f) = Kleisli <$> shrink f

main :: IO ()
main = do
  hspec $ do
    describe "Consensus.Simple" $
      it "checks AST of TH with verbose" $ property $ \a b c d e f ->
        play (depositStagePlayerEpsilon a b d c e) f `assertEq`
        play (depositStagePlayerTH a b d c e) f
   -- it "checks equilibrium of TH with verbose" $ property $ undefined

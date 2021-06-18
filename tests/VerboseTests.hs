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
import Test.Hspec
import Test.QuickCheck

import OpenGames.Preprocessor.THSyntax as THS
import OpenGames.Preprocessor.Preprocessor as Pre
import OpenGames.Preprocessor.TH
import OpenGames.Preprocessor.Compile
import OpenGames.Preprocessor.AbstractSyntax as ABS

import Control.Arrow (Kleisli(..))
import Numeric.Probability.Distribution (certainly, uniform, fromFreqs, T(..))

import Language.Haskell.TH

import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.OpticClass
import OpenGames.Engine.DecisionClass
import OpenGames.Engine.DependentDecision
import OpenGames.Engine.StatefulBayesian hiding (decision, roleDecision, dependentDecision)

generateGame "depositStageTest" ["name", "minDeposit", "maxDeposit", "incrementDeposit", "epsilon"] $
  Block ["costOfCapital"]
        []

  [Line [[|costOfCapital|]]
        []
        [|epsilonDecision epsilon name [minDeposit, minDeposit + incrementDeposit .. maxDeposit]|]
        ["deposit"]
        [[|(-deposit) * costOfCapital|]]]

  [[|deposit|]]
  []

depositStageQuasi name minDeposit maxDeposit incrementDeposit epsilon = [opengame|
  inputs : costOfCapital ;
  feedback : ;

  :---------------------:

  inputs : costOfCapital ;
  feedback : ;
  operation : epsilonDecision epsilon name [minDeposit, minDeposit + incrementDeposit .. maxDeposit] ;
  outputs : deposit ;
  returns : (-deposit) * costOfCapital ;

  :---------------------:

  outputs : deposit ;
  returns : ;
|]

instance (Arbitrary a, Arbitrary b) => (Arbitrary (T a b)) where
  arbitrary = Cons <$> arbitrary
  shrink (Cons ls) = Cons <$> (shrink ls)

instance Arbitrary a => (Arbitrary (StochasticStatefulContext a () Double ())) where
  arbitrary = StochasticStatefulContext @() <$> arbitrary <*> pure (const (const (pure ())))
  shrink = undefined

instance (Arbitrary a, CoArbitrary a, Arbitrary b) => (Arbitrary (Kleisli Stochastic a b)) where
  arbitrary = Kleisli <$> arbitrary
  shrink = undefined

main :: IO ()
main = do
  hspec $ do
    describe "Consensus.Simple" $
      it "checks AST of TH with verbose" $ property $ \a b c d e f g->
        equilibrium (depositStageQuasi a b d c e) f g ==
        equilibrium (depositStageTest a b d c e) f _
   -- it "checks equilibrium of TH with verbose" $ property $ undefined

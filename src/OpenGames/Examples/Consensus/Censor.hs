{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes  #-}
module OpenGames.Examples.Consensus.Censor where

import Control.Arrow (Kleisli(..))
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import OpenGames.Engine.DependentDecision
import OpenGames.Engine.Diagnostics (player)
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.Compile
import Numeric.Probability.Distribution (certainly, fromFreqs, uniform)
import Data.Ord (comparing)
import Data.List (maximumBy)

import Language.Haskell.TH.Syntax

data CensorMove = Honest | Censor deriving (Eq, Ord, Show)
data CarolObservation = Online | Offline deriving (Eq, Ord, Show)
type Deposit = Double

reward, offlinePenalty :: Double
reward = 3
offlinePenalty = 0

carolObservation ::(CensorMove, CensorMove, CensorMove) -> (CarolObservation, CarolObservation, CarolObservation)
carolObservation (Honest, Honest, _) = (Online, Online, Online)
carolObservation (Honest, _, Honest) = (Online, Online, Online)
carolObservation (_, Honest, Honest) = (Online, Online, Online)
carolObservation (x, y, z) = (f x, f y, f z)
  where f Honest = Offline
        f Censor = Online

payoffs :: Double -> (Deposit, Deposit, Deposit) -> (CarolObservation, CarolObservation, CarolObservation) -> (Double, Double, Double)
payoffs _ (x, y, z) (Online, Online, Online) = (reward*x/(x+y+z), reward*y/(x+y+z), reward*z/(x+y+z))
payoffs censorPenalty (x, y, z) (Offline, Online, Online) = (-offlinePenalty*x/(x+y+z), -censorPenalty*y/(x+y+z), -censorPenalty*z/(x+y+z))
payoffs censorPenalty (x, y, z) (Online, Offline, Online) = (-censorPenalty*x/(x+y+z), -offlinePenalty*y/(x+y+z), -censorPenalty*z/(x+y+z))
payoffs censorPenalty (x, y, z) (Online, Online, Offline) = (-censorPenalty*x/(x+y+z), -censorPenalty*y/(x+y+z), -offlinePenalty*z/(x+y+z))

generateGame "depositGame" [] (Block ["costOfCapital"] []
  [Line [] [] [|dependentDecision "Dave"  (const [0.0 .. 10.0])|] ["daveStake"]  [[|-costOfCapital*daveStake|]],
   Line [] [] [|dependentDecision "Erika" (const [0.0 .. 10.0])|] ["erikaStake"] [[|-costOfCapital*erikaStake|]],
   Line [] [] [|dependentDecision "Frank" (const [0.0 .. 10.0])|] ["frankStake"] [[|-costOfCapital*frankStake|]]]
  [[|daveStake|], [|erikaStake|], [|frankStake|]] [])

generateGame "censorGame" ["censorPenalty"] $ Block ["daveStake", "erikaStake", "frankStake", "bribe"] []
  [Line (map param ["daveStake", "bribe"]) [] [|dependentDecision "Dave" (const [Honest, Censor])|] ["daveMove"] [[|case daveMove of {Honest -> davePayoff; Censor -> davePayoff + bribe}|]],
   Line [param "erikaStake"] [] [|dependentDecision "Erika" (const [Honest, Censor])|] ["erikaMove"] [param "erikaPayoff"],
   Line [param "frankStake"] [] [|dependentDecision "Frank" (const [Honest, Censor])|] ["frankMove"] [param "frankPayoff"],
   Line [[|(daveStake, erikaStake, frankStake)|], [|carolObservation (daveMove, erikaMove, frankMove)|]] [] [|fromFunctions (uncurry (payoffs censorPenalty)) id|] ["davePayoff", "erikaPayoff", "frankPayoff"] []]
  [[|daveMove|]] []

generateGame "fullCensorGame" ["censorPenalty"] $ Block [] []
  [Line [[|0.05|]] [] [|depositGame|] ["daveStake", "erikaStake", "frankStake"] [],
   Line (map param ["daveStake", "erikaStake", "frankStake"] ++ [[|0|]]) [] [|censorGame censorPenalty|] ["daveMove"] []]
  [] []

fullCensorGameEq censorPenalty a b c d e f =
  equilibrium (fullCensorGame censorPenalty)
               void ((Kleisli $ const a, Kleisli $ const b, Kleisli $ const c), (Kleisli d, Kleisli e, Kleisli f, ()))

bribeGameSrc = Block ["target", "move", "bribe"] []
  [Line ["target", "()"] [] "roleDecision [()]" ["dummy"] ["case move of {Honest -> 0; Censor -> bribe}"]]
  [] []

generateGame "bribeGame" [] $ Block ["target", "move", "bribe"] []
  [Line [param "target", [|()|]] [] [|roleDecision [()]|] ["dummy"] [[|case move of {Honest -> 0; Censor -> bribe}|]]]
  [] []

generateGame "censorBribeGame" ["censorPenalty", "bribeBudget"] $ Block [] []
  [Line [[|0.05|]] [] [|depositGame|] ["daveStake", "erikaStake", "frankStake"] [],
   Line [] [] [|nature (uniform [0, bribeBudget])|] ["bribe"] [],
   Line (map param ["daveStake", "erikaStake", "frankStake", "bribe"]) [] [|censorGame censorPenalty|] ["daveMove"] []]
   [] []

censorBribeGameEq censorPenalty bribeBudget a b c d e f =
  equilibrium (censorBribeGame censorPenalty bribeBudget)
              void
              ( (Kleisli $ const a, Kleisli $ const b, Kleisli $ const c)
              , ()
              , (Kleisli d, Kleisli e, Kleisli f, ()))

censorStrategy censorPenalty = ((Kleisli $ const $ certainly 10,
                   Kleisli $ const $ certainly 10,
                   Kleisli $ const $ certainly 10),
                   (),
                  (Kleisli $ \(_, bribe) -> let davePayoff daveMove = let (x, _, _) = payoffs censorPenalty (10, 10, 10) (carolObservation (daveMove, Censor, Honest)) in x
                                             in if bribe + davePayoff Censor <= davePayoff Honest then certainly Honest else certainly Censor,
                   Kleisli $ const $ certainly Censor,
                   Kleisli $ const $ certainly Honest,
                   ()))

minimumBribeBudget :: Double -> Double
minimumBribeBudget censorPenalty = minimum [bribeBudget | bribeBudget <- [0, 0.1 .. 10], not $ null (equilibrium (censorBribeGame censorPenalty bribeBudget) void (censorStrategy censorPenalty))]

penaltiesWithMinimumBudget :: [(Double, Double)]
penaltiesWithMinimumBudget = [(censorPenalty, minimumBribeBudget censorPenalty) | censorPenalty <- [0, 0.01 .. 1]]

minimumCensorPenaltyWithMinmaxBudget :: (Double, Double)
minimumCensorPenaltyWithMinmaxBudget = minimum (Prelude.filter (\(x,y) -> y == minmaxBudget) penaltiesWithMinimumBudget)
  where minmaxBudget = snd (maximumBy (comparing snd) penaltiesWithMinimumBudget)

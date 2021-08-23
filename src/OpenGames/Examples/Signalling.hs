{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveGeneric #-}

module OpenGames.Examples.Signalling where

import GHC.Generics
import Numeric.Probability.Distribution
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.Compile
import OpenGames.Preprocessor.AbstractSyntax

import OpenGames.Engine.BayesianDiagnostics

data Productivity = LowProductivity | HighProductivity deriving (Eq, Ord, Show, Generic)
data Effort = LowEffort | HighEffort deriving (Eq, Ord, Show, Generic)
data Wage = LowWage | HighWage deriving (Eq, Ord, Show, Generic)
data Contract = Accept | NotAccept deriving (Eq, Ord, Show)

productivityValuationFirm :: Productivity -> Rational
productivityValuationFirm LowProductivity = 50
productivityValuationFirm HighProductivity = 100

effortValuationWorker :: Productivity -> Effort -> Rational
effortValuationWorker LowProductivity HighEffort = -45
effortValuationWorker _ _ = 0

wageValue :: Wage -> Rational
wageValue LowWage = 40
wageValue HighWage = 80

signallingUtilityWorker :: Productivity -> Effort -> Wage -> Contract -> Rational
signallingUtilityWorker productivity effort wage contract = effortValuationWorker productivity effort + if contract == Accept then wageValue wage else 0

signallingUtilityFirm :: Productivity -> Wage -> Contract -> Rational
signallingUtilityFirm productivity wage Accept = productivityValuationFirm productivity - wageValue wage
signallingUtilityFirm _ _ NotAccept = 0

-- Using TH
generateGame "signallingTH" []
  [Line (Nothing :: Maybe String) []
        []
        [|nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)])|]
        ["productivity"]
        [],
   Line (Nothing :: Maybe String) [param "productivity"]
        []
        [|decision "worker-effort" [LowEffort, HighEffort]|]
        ["effort"]
        [[|signallingUtilityWorker productivity effort wage contract|]],
   Line (Nothing :: Maybe String) [param "effort"]
        []
        [|decision "firm" [LowWage, HighWage]|]
        ["wage"]
        [[|signallingUtilityFirm productivity wage contract|]],
   Line (Nothing :: Maybe String) [param "productivity", param "wage"]
        []
        [|decision "worker-contract" [Accept, NotAccept]|]
        ["contract"]
        [[|signallingUtilityWorker productivity effort wage contract|]]]
--
-- Using Blocks
signallingSrc = Block [] []
                   [Line (Nothing :: Maybe String) [] [] "nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)])" ["productivity"] [],
                    Line (Nothing :: Maybe String) ["productivity"] [] "decision \"worker-effort\" [LowEffort, HighEffort]" ["effort"] ["signallingUtilityWorker productivity effort wage contract"],
                    Line (Nothing :: Maybe String) ["effort"] [] "decision \"firm\" [LowWage, HighWage]" ["wage"] ["signallingUtilityFirm productivity wage contract"],
                    Line (Nothing :: Maybe String) ["productivity", "wage"] [] "decision \"worker-contract\" [Accept, NotAccept]" ["contract"] ["signallingUtilityWorker productivity effort wage contract"]]
                   [] []

-- Using QuasiQuotes
signalling = [game| || =>>
  productivity | <- nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)]) -< | ;

  effort | signallingUtilityWorker productivity effort wage contract
    <- decision "worker-effort" [LowEffort, HighEffort] -< | productivity ;

  wage | signallingUtilityFirm productivity wage contract
    <- decision "firm" [LowWage, HighWage] -< | effort;
  contract | signallingUtilityWorker productivity effort wage contract
    <- decision "worker-contract" [Accept, NotAccept] -< | productivity, wage;
    <<= ||
|]

signallingEquilibrium = equilibrium signalling trivialContext

{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module OpenGames.Examples.Signalling where

import GHC.Generics
import Numeric.Probability.Distribution
import OpenGames.Preprocessor.THSyntax
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
  [line []                                   []
       [|nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)])|]
     ["productivity"] [],
   line [param "productivity"]               []
       [|decision "worker-effort" [LowEffort, HighEffort]|]
     ["effort"]   [[|signallingUtilityWorker productivity effort wage contract|]],
   line [param "effort"]                     []
       [|decision "firm" [LowWage, HighWage]|]
     ["wage"]     [[|signallingUtilityFirm productivity wage contract|]],
   line [param "productivity", param "wage"] []
       [|decision "worker-contract" [Accept, NotAccept]|]
     ["contract"] [[|signallingUtilityWorker productivity effort wage contract|]]]
--
-- Using Blocks
signallingSrc = Block [] []
                   [Line [] [] "nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)])" ["productivity"] [],
                    Line ["productivity"] [] "decision \"worker-effort\" [LowEffort, HighEffort]" ["effort"] ["signallingUtilityWorker productivity effort wage contract"],
                    Line ["effort"] [] "decision \"firm\" [LowWage, HighWage]" ["wage"] ["signallingUtilityFirm productivity wage contract"],
                    Line ["productivity", "wage"] [] "decision \"worker-contract\" [Accept, NotAccept]" ["contract"] ["signallingUtilityWorker productivity effort wage contract"]]
                   [] []

signalling = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(productivity, effort, wage, contract) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)]))))))) >>> (fromFunctions (\((), productivity) -> productivity) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\productivity -> (productivity, productivity)) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "worker-effort" [LowEffort, HighEffort])))))) >>> (fromFunctions (\(productivity, effort) -> (productivity, effort)) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), signallingUtilityWorker productivity effort wage contract)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(productivity, effort) -> ((productivity, effort), effort)) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "firm" [LowWage, HighWage])))))) >>> (fromFunctions (\((productivity, effort), wage) -> (productivity, effort, wage)) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), signallingUtilityFirm productivity wage contract)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(productivity, effort, wage) -> ((productivity, effort, wage), (productivity, wage))) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "worker-contract" [Accept, NotAccept])))))) >>> (fromFunctions (\((productivity, effort, wage), contract) -> (productivity, effort, wage, contract)) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), signallingUtilityWorker productivity effort wage contract))))))))) >>> (fromLens (\(productivity, effort, wage, contract) -> ()) (curry (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract)))))

signallingEquilibrium = equilibrium signalling trivialContext

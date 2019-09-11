module OpenGames.Examples.Signalling where

import Numeric.Probability.Distribution

import OpenGames.Engine.BayesianDiagnostics

data Productivity = LowProductivity | HighProductivity deriving (Eq, Ord, Show)
data Effort = LowEffort | HighEffort deriving (Eq, Ord, Show)
data Wage = LowWage | HighWage deriving (Eq, Ord, Show)
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

signalling = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(productivity, effort, wage, contract) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)]))))))) >>> (fromFunctions (\((), productivity) -> productivity) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\productivity -> (productivity, productivity)) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "worker-effort" [LowEffort, HighEffort])))))) >>> (fromFunctions (\(productivity, effort) -> (productivity, effort)) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), signallingUtilityWorker productivity effort wage contract)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(productivity, effort) -> ((productivity, effort), effort)) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "firm" [LowWage, HighWage])))))) >>> (fromFunctions (\((productivity, effort), wage) -> (productivity, effort, wage)) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), signallingUtilityFirm productivity wage contract)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(productivity, effort, wage) -> ((productivity, effort, wage), (productivity, wage))) (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "worker-contract" [Accept, NotAccept])))))) >>> (fromFunctions (\((productivity, effort, wage), contract) -> (productivity, effort, wage, contract)) (\(productivity, effort, wage, contract) -> ((productivity, effort, wage, contract), signallingUtilityWorker productivity effort wage contract))))))))) >>> (fromLens (\(productivity, effort, wage, contract) -> ()) (curry (\((productivity, effort, wage, contract), ()) -> (productivity, effort, wage, contract)))))

signallingEquilibrium = equilibrium signalling trivialContext

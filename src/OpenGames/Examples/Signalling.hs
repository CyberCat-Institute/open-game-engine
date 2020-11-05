module OpenGames.Examples.Signalling where

import Numeric.Probability.Distribution
import OpenGames.Engine.BayesianDiagnostics

-- Spence's signalling game

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

-- Beer-quiche game

data Strength = Weak | Strong deriving (Eq, Ord, Show)
data Food = Beer | Quiche deriving (Eq, Ord, Show)
data Fight = Duel | NoDuel deriving (Eq, Ord, Show)

signallingUtilitySender :: Strength -> Food -> Fight -> Rational
signallingUtilitySender strength food fight = (if (strength == Weak && food == Quiche) || (strength == Strong && food == Beer) then 1 else 0)
                                              + if fight == NoDuel then 2 else 0

signallingUtilityReceiver :: Strength -> Fight -> Rational
signallingUtilityReceiver strength fight = if (strength == Weak && fight == Duel) || (strength == Strong && fight == NoDuel) then 1
                                           else 0

beerquiche = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(strength, food, fight) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((strength, food, fight), ()) -> (strength, food, fight))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(Weak, 3), (Strong, 7)]))))))) >>> (fromFunctions (\((), strength) -> strength) (\(strength, food, fight) -> ((strength, food, fight), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\strength -> (strength, strength)) (\((strength, food, fight), ()) -> (strength, food, fight))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "food-choice" [Beer, Quiche])))))) >>> (fromFunctions (\(strength, food) -> (strength, food)) (\(strength, food, fight) -> ((strength, food, fight), signallingUtilitySender strength food fight)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(strength, food) -> ((strength, food), food)) (\((strength, food, fight), ()) -> (strength, food, fight))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "receiver" [Duel, NoDuel])))))) >>> (fromFunctions (\((strength, food), fight) -> (strength, food, fight)) (\(strength, food, fight) -> ((strength, food, fight), signallingUtilityReceiver strength fight))))))))) >>> (fromLens (\(strength, food, fight) -> ()) (curry (\((strength, food, fight), ()) -> (strength, food, fight)))))

beerquicheEquilibrium = equilibrium beerquiche trivialContext

-- A simpler version of the education game

data Mind = Clever | Normal deriving (Eq, Ord, Show)
data Education = Degree | NoDegree deriving (Eq, Ord, Show)
data Decision = Hire | NoHire deriving (Eq, Ord, Show)

signallingUtilityApplicant :: Mind -> Education -> Decision -> Rational
signallingUtilityApplicant mind edu dec = (if mind == Clever && edu == Degree then 0
                                           else if mind == Clever && edu == NoDegree then 1
                                           else if mind == Normal && edu == Degree then -1
                                           else 1)
                                          + if dec == Hire then 0.5 else (-1)

signallingUtilityCompany :: Mind -> Decision -> Rational
signallingUtilityCompany mind dec = if dec == NoHire then 0
                                    else if mind == Clever then 2
                                    else -1

educationgame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(mind, edu, dec) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((mind, edu, dec), ()) -> (mind, edu, dec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(Clever, 3), (Normal, 7)]))))))) >>> (fromFunctions (\((), mind) -> mind) (\(mind, edu, dec) -> ((mind, edu, dec), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\mind -> (mind, mind)) (\((mind, edu, dec), ()) -> (mind, edu, dec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "edu-choice" [Degree, NoDegree])))))) >>> (fromFunctions (\(mind, edu) -> (mind, edu)) (\(mind, edu, dec) -> ((mind, edu, dec), signallingUtilityApplicant mind edu dec)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(mind, edu) -> ((mind, edu), edu)) (\((mind, edu, dec), ()) -> (mind, edu, dec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "company" [Hire, NoHire])))))) >>> (fromFunctions (\((mind, edu), dec) -> (mind, edu, dec)) (\(mind, edu, dec) -> ((mind, edu, dec), signallingUtilityCompany mind dec))))))))) >>> (fromLens (\(mind, edu, dec) -> ()) (curry (\((mind, edu, dec), ()) -> (mind, edu, dec)))))

educationgameEquilibrium = equilibrium educationgame trivialContext

-- Looking for a roommate

data Mood = Funny | Serious deriving (Eq, Ord, Show)
data Clown = On | Off deriving (Eq, Ord, Show)
data Casting = Offer | Deny deriving (Eq, Ord, Show)

signallingUtilityRoommate :: Mood -> Clown -> Casting -> Rational
signallingUtilityRoommate mood clown casting = (if mood == Funny && clown == On then 5
                                                else if mood == Funny && clown == Off then 0
                                                else if mood == Serious && clown == On then -5
                                                else 0)
                                               + if casting == Offer then 4

signallingUtilityCoop :: Mood -> Casting -> Rational
signallingUtilityCoop mood casting = if mood == Serious then 0
                                     else if casting == Offer then 4
									 else 0

roommategame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(mood, clown, casting) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((mood, clown, casting), ()) -> (mood, clown, casting))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (fromFreqs [(Funny, 3), (Serious, 2)]))))))) >>> (fromFunctions (\((), mood) -> mood) (\(mood, clown, casting) -> ((mood, clown, casting), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\mood -> (mood, mood)) (\((mood, clown, casting), ()) -> (mood, clown, casting))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "clown-choice" [On, Off])))))) >>> (fromFunctions (\(mood, clown) -> (mood, clown)) (\(mood, clown, casting) -> ((mood, clown, casting), signallingUtilityRoommate mood clown casting)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(mood, clown) -> ((mood, clown), clown)) (\((mood, clown, casting), ()) -> (mood, clown, casting))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "coop" [Offer, Deny])))))) >>> (fromFunctions (\((mood, clown), casting) -> (mood, clown, casting)) (\(mood, clown, casting) -> ((mood, clown, casting), signallingUtilityCoop mood casting))))))))) >>> (fromLens (\(mood, clown, casting) -> ()) (curry (\((mood, clown, casting), ()) -> (mood, clown, casting)))))

roommategameEquilibrium = equilibrium roommategame trivialContext

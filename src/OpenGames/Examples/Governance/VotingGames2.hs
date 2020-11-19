{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.Governance.VotingGames2 where

import           Control.Arrow (Kleisli(..))
import           Numeric.Probability.Distribution


import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import           OpenGames.Engine.DependentDecision

import           OpenGames.Preprocessor.AbstractSyntax

--------------------------
-- 0. Payoffs
-- 0.0 Share-cropping

productionFunction :: Double -> Double -> Double
productionFunction tech effort = tech*effort

payoffWorkerShare,payoffLandLordShare :: Double -> Double -> Double
payoffWorkerShare amount share = amount*share
payoffLandLordShare amount share = amount *(1-share)

-- 0.1 wage setting game
payoffLandLordWage :: Double -> Double -> Double
payoffLandLordWage amount wage = amount - wage

payoffWorkerWage :: Double -> Double
payoffWorkerWage wage  = wage


-- 0.2 voting function
majority :: Double -> Either () () -> Either () () -> Either Double Double
majority tech (Left ()) (Left ()) = Left tech
majority tech (Right ()) (Right ()) = Right tech
majority tech _ _ = Left tech

-- 0.3 random choice of institutional
randomRegime :: Double -> [Char] -> Either Double Double
randomRegime tech "cr" = Left tech
randomTegime tech "wa" = Right tech

--------------------------
-- 1. Preliminary Stage
-- Nature determining the technology 

technologySrc = Block [] []
                     [Line [] [] "nature (uniform [0, 0.1 .. 1])" ["tech"] []]
                     ["tech"] []

technology = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\tech -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\(tech, ()) -> tech)) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0, 0.1 .. 1]))))))) >>> (fromFunctions (\((), tech) -> tech) (\tech -> (tech, ())))))))) >>> (fromLens (\tech -> tech) (curry (\(tech, ()) -> tech))))

--------------------------
-- 2. Administrative Stage
-- 2.0 Landowners setting the share 

landLordShareSrc = Block ["tech"] []
                  [Line ["tech"] [] "dependentDecision \"landLord\" (const [0, 0.1 .. 1])" ["share"] ["payoffLandLordShare (productionFunction tech effort) share"]]
                  ["share"] ["effort"]


landLordShare = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "landLord" (const [0, 0.1 .. 1]))))))) >>> (fromFunctions (\(tech, share) -> (tech, share)) (\(tech, share, effort) -> ((tech, share, effort), payoffLandLordShare (productionFunction tech effort) share)))))))) >>> (fromLens (\(tech, share) -> share) (curry (\((tech, share), effort) -> (tech, share, effort)))))


-- 2.1 Landowners setting the share 

landLordWageSrc = Block ["tech"] []
                  [Line ["tech"] [] "dependentDecision \"landLord\" (const [0, 1 .. 10])" ["wage"] ["payoffLandLordWage (productionFunction tech effort) wage"]]
                  ["wage"] ["effort"]


landLordWage = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, wage, effort) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, wage, effort), ()) -> (tech, wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "landLord" (const [0, 1 .. 10]))))))) >>> (fromFunctions (\(tech, wage) -> (tech, wage)) (\(tech, wage, effort) -> ((tech, wage, effort), payoffLandLordWage (productionFunction tech effort) wage)))))))) >>> (fromLens (\(tech, wage) -> wage) (curry (\((tech, wage), effort) -> (tech, wage, effort)))))



--------------------------
-- 3. Substantive Stage
-- 3.0 Workers spending effort

workerShareSrc = Block ["tech","share"] []
                  [Line ["(tech,share)"] [] "dependentDecision \"worker\" (const [1, 1 .. 10])" ["effort"] ["payoffWorkerShare (productionFunction tech effort) share"]]
                  ["effort"] []

workerShare = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, share) -> ((tech, share), (tech,share))) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "worker" (const [1, 1 .. 10]))))))) >>> (fromFunctions (\((tech, share), effort) -> (tech, share, effort)) (\(tech, share, effort) -> ((tech, share, effort), payoffWorkerShare (productionFunction tech effort) share)))))))) >>> (fromLens (\(tech, share, effort) -> effort) (curry (\((tech, share, effort), ()) -> (tech, share, effort)))))

-- 3.1 Substantive Stage
-- Workers spending effort

workerWageSrc = Block ["wage"] []
                  [Line ["wage"] [] "dependentDecision \"worker\" (const [1, 1 .. 10])" ["effort"] ["payoffWorkerWage wage"]]
                  ["effort"] []

workerWage = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(wage, effort) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\wage -> (wage, wage)) (\((wage, effort), ()) -> (wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "worker" (const [1, 1 .. 10]))))))) >>> (fromFunctions (\(wage, effort) -> (wage, effort)) (\(wage, effort) -> ((wage, effort), payoffWorkerWage wage)))))))) >>> (fromLens (\(wage, effort) -> effort) (curry (\((wage, effort), ()) -> (wage, effort)))))


--------------------------
-- 4. Partial continuation games for each institutional arrangement
-- 4.0 share-cropping

cropGameSrc = Block ["tech"] []
                  [Line ["tech"] [] "landLordShare" ["share"] ["effort"],
                   Line ["tech","share"] [] "workerShare" ["effort"] []]
                  ["effort"] []

cropGame =reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((landLordShare)))))) >>> (fromFunctions (\(tech, share) -> (tech, share)) (\(tech, share, effort) -> ((tech, share, effort), effort))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, share) -> ((tech, share), (tech, share))) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((workerShare)))))) >>> (fromFunctions (\((tech, share), effort) -> (tech, share, effort)) (\(tech, share, effort) -> ((tech, share, effort), ()))))))))) >>> (fromLens (\(tech, share, effort) -> effort) (curry (\((tech, share, effort), ()) -> (tech, share, effort)))))

-- 4.1 wage setting
wageGameSrc = Block ["tech"] []
                  [Line ["tech"] [] "landLordWage" ["wage"] ["effort"],
                   Line ["wage"] [] "workerWage" ["effort"] []]
                  ["effort"] []

wageGame =  reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, wage, effort) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, wage, effort), ()) -> (tech, wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((landLordWage)))))) >>> (fromFunctions (\(tech, wage) -> (tech, wage)) (\(tech, wage, effort) -> ((tech, wage, effort), effort))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, wage) -> ((tech, wage), wage)) (\((tech, wage, effort), ()) -> (tech, wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((workerWage)))))) >>> (fromFunctions (\((tech, wage), effort) -> (tech, wage, effort)) (\(tech, wage, effort) -> ((tech, wage, effort), ()))))))))) >>> (fromLens (\(tech, wage, effort) -> effort) (curry (\((tech, wage, effort), ()) -> (tech, wage, effort)))))

--------------------------
-- 5  Voting game

votingInstSrc = Block [] []
                    [Line [] [] "dependentDecision \"gov1\" (const [Left (), Right ()])" ["vote1"] ["0"],
                     Line [] [] "dependentDecision \"gov2\" (const [Left (), Right ()])" ["vote2"] ["0"]]
                    ["vote1","vote2"] []

votingInst =reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2), ()) -> (vote1, vote2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "gov1" (const [Left (), Right ()]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2) -> ((vote1, vote2), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2), ()) -> (vote1, vote2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "gov2" (const [Left (), Right ()]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2) -> ((vote1, vote2), 0))))))))) >>> (fromLens (\(vote1, vote2) -> (vote1, vote2)) (curry (\((vote1, vote2), ()) -> (vote1, vote2)))))

--------------------------
-- 6. Putting the whole game together
-- 6.0 Simple share-cropping

cropGameCompleteSrc = Block [] []
                 [Line [] [] "technology" ["tech"] [],
                  Line ["tech"] [] "landLordShare" ["share"] ["effort"],
                  Line ["tech","share"] [] "workerShare" ["effort"] []]
                  [] []

cropGameComplete = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((technology)))))) >>> (fromFunctions (\((), tech) -> tech) (\(tech, share, effort) -> ((tech, share, effort), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((landLordShare)))))) >>> (fromFunctions (\(tech, share) -> (tech, share)) (\(tech, share, effort) -> ((tech, share, effort), effort)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, share) -> ((tech, share), (tech, share))) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((workerShare)))))) >>> (fromFunctions (\((tech, share), effort) -> (tech, share, effort)) (\(tech, share, effort) -> ((tech, share, effort), ()))))))))) >>> (fromLens (\(tech, share, effort) -> ()) (curry (\((tech, share, effort), ()) -> (tech, share, effort)))))


-- 6.1 Simple wage-setting


wageGameCompleteSrc = Block [] []
                 [Line [] [] "technology" ["tech"] [],
                  Line ["tech"] [] "landLordWage" ["wage"] ["effort"],
                  Line ["wage"] [] "workerWage" ["effort"] []]
                  [] []

wageGameComplete = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, wage, effort) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((tech, wage, effort), ()) -> (tech, wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((technology)))))) >>> (fromFunctions (\((), tech) -> tech) (\(tech, wage, effort) -> ((tech, wage, effort), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, wage, effort), ()) -> (tech, wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((landLordWage)))))) >>> (fromFunctions (\(tech, wage) -> (tech, wage)) (\(tech, wage, effort) -> ((tech, wage, effort), effort)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, wage) -> ((tech, wage), wage)) (\((tech, wage, effort), ()) -> (tech, wage, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((workerWage)))))) >>> (fromFunctions (\((tech, wage), effort) -> (tech, wage, effort)) (\(tech, wage, effort) -> ((tech, wage, effort), ()))))))))) >>> (fromLens (\(tech, wage, effort) -> ()) (curry (\((tech, wage, effort), ()) -> (tech, wage, effort)))))

-- 6.2 Branching game with actual choice
branchingGameSrc = Block [] []
                 [Line [] [] "technology" ["tech"] [],
                  Line [] [] "votingInst" ["vote1","vote2"] [],
                  Line ["majority tech vote1 vote2"] [] "cropGame +++ wageGame " ["discard"] []
                 ]
                 [] []

branchingGame  = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, vote1, vote2, discard) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((tech, vote1, vote2, discard), ()) -> (tech, vote1, vote2, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((technology)))))) >>> (fromFunctions (\((), tech) -> tech) (\(tech, vote1, vote2, discard) -> ((tech, vote1, vote2, discard), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, ())) (\((tech, vote1, vote2, discard), ()) -> (tech, vote1, vote2, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((votingInst)))))) >>> (fromFunctions (\(tech, (vote1, vote2)) -> (tech, vote1, vote2)) (\(tech, vote1, vote2, discard) -> ((tech, vote1, vote2, discard), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, vote1, vote2) -> ((tech, vote1, vote2), majority tech vote1 vote2)) (\((tech, vote1, vote2, discard), ()) -> (tech, vote1, vote2, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((cropGame +++ wageGame )))))) >>> (fromFunctions (\((tech, vote1, vote2), discard) -> (tech, vote1, vote2, discard)) (\(tech, vote1, vote2, discard) -> ((tech, vote1, vote2, discard), ()))))))))) >>> (fromLens (\(tech, vote1, vote2, discard) -> ()) (curry (\((tech, vote1, vote2, discard), ()) -> (tech, vote1, vote2, discard))))) 


-- 6.3 Random evolution of institution
randomInstSrc = Block [] []
                 [Line [] [] "technology" ["tech"] [],
                  Line [] [] "nature (uniform [\"cr\", \"wa\"])" ["inst"] [],
                  Line ["randomRegime tech inst"] [] "cropGame +++ wageGame " ["discard"] []
                 ]
                 [] []

randomInst = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, inst, discard) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((tech, inst, discard), ()) -> (tech, inst, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((technology)))))) >>> (fromFunctions (\((), tech) -> tech) (\(tech, inst, discard) -> ((tech, inst, discard), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, ())) (\((tech, inst, discard), ()) -> (tech, inst, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform ["cr", "wa"]))))))) >>> (fromFunctions (\(tech, inst) -> (tech, inst)) (\(tech, inst, discard) -> ((tech, inst, discard), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, inst) -> ((tech, inst), randomRegime tech inst)) (\((tech, inst, discard), ()) -> (tech, inst, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((cropGame +++ wageGame )))))) >>> (fromFunctions (\((tech, inst), discard) -> (tech, inst, discard)) (\(tech, inst, discard) -> ((tech, inst, discard), ()))))))))) >>> (fromLens (\(tech, inst, discard) -> ()) (curry (\((tech, inst, discard), ()) -> (tech, inst, discard)))))


-- 6 eq checks
eqCropGame = OpenGames.Engine.OpticClass.equilibrium cropGameComplete void
eqWageGame = OpenGames.Engine.OpticClass.equilibrium wageGameComplete void
eqBranchGame = OpenGames.Engine.OpticClass.equilibrium branchingGame void
eqRandomInstGame = OpenGames.Engine.OpticClass.equilibrium randomInst void

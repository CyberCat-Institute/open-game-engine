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

productionFunction :: Double -> Double -> Double
productionFunction tech effort = tech*effort

payoffWorker,payoffLandLord :: Double -> Double -> Double
payoffWorker amount share = amount*share
payoffLandLord amount share = amount *(1-share)

--------------------------
-- 1. Preliminary Stage
-- Nature determining the technology 

technologySrc = Block [] []
                     [Line [] [] "nature (uniform [0, 0.1 .. 1])" ["tech"] []]
                     ["tech"] []

technology = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\tech -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\(tech, ()) -> tech)) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [0, 0.1 .. 1]))))))) >>> (fromFunctions (\((), tech) -> tech) (\tech -> (tech, ())))))))) >>> (fromLens (\tech -> tech) (curry (\(tech, ()) -> tech))))

--------------------------
-- 2. Administrative Stage
-- Landowners setting the share 

landLordSrc = Block ["tech"] []
                  [Line ["tech"] [] "dependentDecision \"landLord\" (const [0, 0.1 .. 1])" ["share"] ["payoffLandLord (productionFunction tech effort) share"]]
                  ["share"] ["effort"]


landLord = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "landLord" (const [0, 0.1 .. 1]))))))) >>> (fromFunctions (\(tech, share) -> (tech, share)) (\(tech, share, effort) -> ((tech, share, effort), payoffLandLord (productionFunction tech effort) share)))))))) >>> (fromLens (\(tech, share) -> share) (curry (\((tech, share), effort) -> (tech, share, effort)))))


--------------------------
-- 3. Substantive Stage
-- Workers spending effort

workerSrc = Block ["tech","share"] []
                  [Line ["(tech,share)"] [] "dependentDecision \"worker\" (const [1, 1 .. 10])" ["effort"] ["payoffWorker (productionFunction tech effort) share"]]
                  ["effort"] []

worker = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, share) -> ((tech, share), (tech,share))) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "worker" (const [1, 1 .. 10]))))))) >>> (fromFunctions (\((tech, share), effort) -> (tech, share, effort)) (\(tech, share, effort) -> ((tech, share, effort), payoffWorker (productionFunction tech effort) share)))))))) >>> (fromLens (\(tech, share, effort) -> effort) (curry (\((tech, share, effort), ()) -> (tech, share, effort)))))

--------------------------
-- 4. Putting the whole game together


cropGameSrc = Block [] []
                 [Line [] [] "technology" ["tech"] [],
                  Line ["tech"] [] "landLord" ["share"] ["effort"],
                  Line ["tech","share"] [] "worker" ["effort"] []]
                  [] []

cropGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(tech, share, effort) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((technology)))))) >>> (fromFunctions (\((), tech) -> tech) (\(tech, share, effort) -> ((tech, share, effort), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\tech -> (tech, tech)) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((landLord)))))) >>> (fromFunctions (\(tech, share) -> (tech, share)) (\(tech, share, effort) -> ((tech, share, effort), effort)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(tech, share) -> ((tech, share), (tech, share))) (\((tech, share, effort), ()) -> (tech, share, effort))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((worker)))))) >>> (fromFunctions (\((tech, share), effort) -> (tech, share, effort)) (\(tech, share, effort) -> ((tech, share, effort), ()))))))))) >>> (fromLens (\(tech, share, effort) -> ()) (curry (\((tech, share, effort), ()) -> (tech, share, effort)))))


equilibriumCropGame = OpenGames.Engine.OpticClass.equilibrium cropGame void

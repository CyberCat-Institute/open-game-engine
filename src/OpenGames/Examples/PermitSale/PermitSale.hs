{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.PermitSale.PermitSale where

import qualified Control.Arrow as CA
import           Numeric.Probability.Distribution
--import           OpenGames.Engine.BayesianDiagnostics
import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian
import           OpenGames.Preprocessor.AbstractSyntax
import           OpenGames.Preprocessor.Preprocessor


-------------------------------
--- 0 Types

type Permit = Double
type Value  = Double
type Cost   = Double
type Profit = Double



-- Companies payoffs
-- Production requires fixed number of permits. Required number of permits are private information
-- NOTE We should consider other settings, value variable and private information etc. but before we need to fix the game structure and the possible dependency of the action spaces.
profit :: Value -> Permit -> Permit -> Permit -> Cost -> Profit
profit v reqP receivedP usedP c | receivedP < reqP                  =   - c*usedP
                                -- ^ If not enough permits received, using any of them results in costs
                                | receivedP >= reqP && usedP < reqP =   - c*usedP
                                -- ^ If enough permits received, but using not the required amount only causes costs
                                | otherwise                         = v - c*usedP

-- companies: number of permits needed for production
prior :: Stochastic Permit
prior =  uniform [1,2]


--------------------------------
-- 1. Concrete parameterizations
value :: Value
value = 3

cost :: Cost
cost  = 2

availablePermits :: Permit
availablePermits = 2


randomAll :: Permit -> Stochastic (Permit,Permit)
randomAll noP = do
       p1 <- uniform [0..noP]
       p2 <- certainly (noP - p1)
       return (p1,p2)

-------------------------------
-- 2. Initial allocation phase
-- random allocation of permits

randomAllocationSrc = Block [] []
            [Line [] [] "nature prior" ["pType1"] [],
             Line [] [] "nature prior" ["pType2"] [],
             Line [] [] "nature (randomAll availablePermits)" ["p1", "p2"] []]
            ["p1","p2","pType1","pType2"] []

randomAllocation = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(pType1, pType2, p1, p2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((pType1, pType2, p1, p2), ()) -> (pType1, pType2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature prior)))))) >>> (fromFunctions (\((), pType1) -> pType1) (\(pType1, pType2, p1, p2) -> ((pType1, pType2, p1, p2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\pType1 -> (pType1, ())) (\((pType1, pType2, p1, p2), ()) -> (pType1, pType2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature prior)))))) >>> (fromFunctions (\(pType1, pType2) -> (pType1, pType2)) (\(pType1, pType2, p1, p2) -> ((pType1, pType2, p1, p2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(pType1, pType2) -> ((pType1, pType2), ())) (\((pType1, pType2, p1, p2), ()) -> (pType1, pType2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (randomAll availablePermits))))))) >>> (fromFunctions (\((pType1, pType2), (p1, p2)) -> (pType1, pType2, p1, p2)) (\(pType1, pType2, p1, p2) -> ((pType1, pType2, p1, p2), ()))))))))) >>> (fromLens (\(pType1, pType2, p1, p2) -> (p1, p2, pType1, pType2)) (curry (\((pType1, pType2, p1, p2), ()) -> (pType1, pType2, p1, p2)))))

-------------------------------
-- 3. Production continuation games
-- Without feeding information forward

productionDecSrc = Block ["p1", "p2", "pType1", "pType2"] []
  [Line ["\"player1\"", "[0 .. p1]", "(p1, pType1)"] [] "dependentDecision" ["dec1"] ["profit value pType1 p1 dec1 cost"],
   Line ["\"player2\"", "[0 .. p2]", "(p2, pType2)"] [] "dependentDecision" ["dec2"] ["profit value pType2 p2 dec2 cost"]]
  [] []

productionDec = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, pType1, pType2, dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), ("player1", [0 .. p1], (p1, pType1)))) (\((p1, p2, pType1, pType2, dec1, dec2), ()) -> (p1, p2, pType1, pType2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, pType1, pType2), dec1) -> (p1, p2, pType1, pType2, dec1)) (\(p1, p2, pType1, pType2, dec1, dec2) -> ((p1, p2, pType1, pType2, dec1, dec2), profit value pType1 p1 dec1 cost))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, pType1, pType2, dec1) -> ((p1, p2, pType1, pType2, dec1), ("player2", [0 .. p2], (p2, pType2)))) (\((p1, p2, pType1, pType2, dec1, dec2), ()) -> (p1, p2, pType1, pType2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, pType1, pType2, dec1), dec2) -> (p1, p2, pType1, pType2, dec1, dec2)) (\(p1, p2, pType1, pType2, dec1, dec2) -> ((p1, p2, pType1, pType2, dec1, dec2), profit value pType2 p2 dec2 cost))))))))) >>> (fromLens (\(p1, p2, pType1, pType2, dec1, dec2) -> ()) (curry (\((p1, p2, pType1, pType2, dec1, dec2), ()) -> (p1, p2, pType1, pType2, dec1, dec2)))))


-------------------------------
-- 4. Resale market


------------------------------
-- 5. Complete game
-- For now excluding a resale market


completeGameSrc = Block [] []
  [Line [] [] "randomAllocation" ["p1","p2","pType1","pType2"] [],
   Line ["p1","p2","pType1","pType2"] [] "productionDec" [] []]
  [] []

completeGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, pType1, pType2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((randomAllocation)))))) >>> (fromFunctions (\((), (p1, p2, pType1, pType2)) -> (p1, p2, pType1, pType2)) (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), (p1, p2, pType1, pType2))) (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec)))))) >>> (fromFunctions (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2)) (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), ()))))))))) >>> (fromLens (\(p1, p2, pType1, pType2) -> ()) (curry (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2)))))


------------------------------
-- 6. Analysis
eqGame =  equilibrium completeGame void

strategyPermit (recP,reqP) | recP < reqP         = certainly 0
                     -- ^ If not sufficient permits received produce nothing
                     | recP >= reqP && reqP == 1 = certainly 1
                     -- ^ If sufficient permits received and efficient production, use permits
                     | otherwise                 = certainly 0
                     -- ^ If sufficient permits received but inefficient production, do not use permits.


strategyCopy (recP,_) = certainly recP

-- eq: eqGame (((),(),()),((CA.Kleisli strategyPermit, CA.Kleisli strategyPermit)))
-- not an eq: eqGame (((),(),()),((CA.Kleisli strategyPermit, CA.Kleisli strategyCopy)))

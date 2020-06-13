{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.PermitSale.PermitSale where

import Numeric.Probability.Distribution
import OpenGames.Engine.BayesianDiagnostics
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Preprocessor


-------------------------------
--- 0 Types

type Permit = Rational
type Value  = Rational
type Cost   = Rational
type Profit = Rational



-- companies payoffs
-- Production requires fixed number of permits. Required number of permits are private information
-- NOTE We should consider other settings, value variable and private information etc. but before we need to fix the game structure and the possible dependency of the action spaces.
profit :: Value -> Permit -> Permit -> Permit -> Cost -> Profit
profit v reqP receivedP usedP c | receivedP < reqP                  =   - c*usedP
                                -- ^ If not permits received, using any of them results in costs
                                | receivedP >= reqP && usedP < reqP =   - c*usedP
                                -- ^ If enough permits received, but using not the required amount only causes costs
                                | otherwise                         = v - c*usedP
                                

-- companies: number of permits needed for production
prior :: T Rational Permit
prior =  uniform [1,2]


--------------------------------
-- 1. Concrete parameterizations
value :: Value
value = 3

cost :: Cost
cost  = 2

availablePermits :: Permit
availablePermits = 2 


randomAll :: Permit -> T Rational (Permit,Permit)
randomAll noP = do
       p1 <- uniform [0..noP]
       p2 <- uniform [(noP - p1)..noP]
       return (p1,p2)

-------------------------------
-- 2. Initial allocation phase
-- random allocation of permits

randomAllocationSrc = Block [] []
            [Line [] [] "nature prior" ["pType1"] [],
             Line [] [] "nature prior" ["pType2"] [],
             Line [] [] "nature (randomAll availablePermits)" ["(p1,p2)"] []]
            ["p1","p2","pType1","pType2"] []

randomAllocation = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(pType1, pType2, (p1,p2)) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((pType1, pType2, (p1,p2)), ()) -> (pType1, pType2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature prior)))))) >>> (fromFunctions (\((), pType1) -> pType1) (\(pType1, pType2, (p1,p2)) -> ((pType1, pType2, (p1,p2)), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\pType1 -> (pType1, ())) (\((pType1, pType2, (p1,p2)), ()) -> (pType1, pType2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature prior)))))) >>> (fromFunctions (\(pType1, pType2) -> (pType1, pType2)) (\(pType1, pType2, (p1,p2)) -> ((pType1, pType2, (p1,p2)), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(pType1, pType2) -> ((pType1, pType2), ())) (\((pType1, pType2, (p1,p2)), ()) -> (pType1, pType2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (randomAll availablePermits))))))) >>> (fromFunctions (\((pType1, pType2), (p1,p2)) -> (pType1, pType2, (p1,p2))) (\(pType1, pType2, (p1,p2)) -> ((pType1, pType2, (p1,p2)), ()))))))))) >>> (fromLens (\(pType1, pType2, (p1,p2)) -> (p1, p2, pType1, pType2)) (curry (\((pType1, pType2, (p1,p2)), ()) -> (pType1, pType2, (p1,p2))))))



-------------------------------
-- 3. Production continuation games
-- Without feeding information forward
productionDec02Src = Block ["(p1,p2,pType1,pType2)"] []
                    [Line ["p1","pType1"] [] "decision \"player1\" [0]"     ["dec1"] ["profit value pType1 p1 dec1 cost"],
                     Line ["p2","pType2"] [] "decision \"player2\" [0,1,2]" ["dec2"] ["profit value pType2 p2 dec2 cost"]]
                     [] []

productionDec02 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\((p1,p2,pType1,pType2), dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1,p2,pType1,pType2) -> ((p1,p2,pType1,pType2), (p1, pType1))) (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [0])))))) >>> (fromFunctions (\((p1,p2,pType1,pType2), dec1) -> ((p1,p2,pType1,pType2), dec1)) (\((p1,p2,pType1,pType2), dec1, dec2) -> (((p1,p2,pType1,pType2), dec1, dec2), profit value pType1 p1 dec1 cost))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((p1,p2,pType1,pType2), dec1) -> (((p1,p2,pType1,pType2), dec1), (p2, pType2))) (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [0,1,2])))))) >>> (fromFunctions (\(((p1,p2,pType1,pType2), dec1), dec2) -> ((p1,p2,pType1,pType2), dec1, dec2)) (\((p1,p2,pType1,pType2), dec1, dec2) -> (((p1,p2,pType1,pType2), dec1, dec2), profit value pType2 p2 dec2 cost))))))))) >>> (fromLens (\((p1,p2,pType1,pType2), dec1, dec2) -> ()) (curry (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2)))))

productionDec11Src = Block ["(p1,p2,pType1,pType2)"] []
                    [Line ["p1","pType1"] [] "decision \"player1\" [0,1]" ["dec1"] ["profit value pType1 p1 dec1 cost"],
                     Line ["p2","pType2"] [] "decision \"player2\" [0,1]" ["dec2"] ["profit value pType2 p2 dec2 cost"]]
                     [] []

productionDec11 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\((p1,p2,pType1,pType2), dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1,p2,pType1,pType2) -> ((p1,p2,pType1,pType2), (p1, pType1))) (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [0,1])))))) >>> (fromFunctions (\((p1,p2,pType1,pType2), dec1) -> ((p1,p2,pType1,pType2), dec1)) (\((p1,p2,pType1,pType2), dec1, dec2) -> (((p1,p2,pType1,pType2), dec1, dec2), profit value pType1 p1 dec1 cost))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((p1,p2,pType1,pType2), dec1) -> (((p1,p2,pType1,pType2), dec1), (p2, pType2))) (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [0,1])))))) >>> (fromFunctions (\(((p1,p2,pType1,pType2), dec1), dec2) -> ((p1,p2,pType1,pType2), dec1, dec2)) (\((p1,p2,pType1,pType2), dec1, dec2) -> (((p1,p2,pType1,pType2), dec1, dec2), profit value pType2 p2 dec2 cost))))))))) >>> (fromLens (\((p1,p2,pType1,pType2), dec1, dec2) -> ()) (curry (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2)))))


productionDec20Src = Block ["(p1,p2,pType1,pType2)"] []
                    [Line ["p1","pType1"] [] "decision \"player1\" [0,1,2]" ["dec1"] ["profit value pType1 p1 dec1 cost"],
                     Line ["p2","pType2"] [] "decision \"player2\" [0]"     ["dec2"] ["profit value pType2 p2 dec2 cost"]]
                     [] []

productionDec20 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\((p1,p2,pType1,pType2), dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1,p2,pType1,pType2) -> ((p1,p2,pType1,pType2), (p1, pType1))) (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [0,1,2])))))) >>> (fromFunctions (\((p1,p2,pType1,pType2), dec1) -> ((p1,p2,pType1,pType2), dec1)) (\((p1,p2,pType1,pType2), dec1, dec2) -> (((p1,p2,pType1,pType2), dec1, dec2), profit value pType1 p1 dec1 cost))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((p1,p2,pType1,pType2), dec1) -> (((p1,p2,pType1,pType2), dec1), (p2, pType2))) (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [0])))))) >>> (fromFunctions (\(((p1,p2,pType1,pType2), dec1), dec2) -> ((p1,p2,pType1,pType2), dec1, dec2)) (\((p1,p2,pType1,pType2), dec1, dec2) -> (((p1,p2,pType1,pType2), dec1, dec2), profit value pType2 p2 dec2 cost))))))))) >>> (fromLens (\((p1,p2,pType1,pType2), dec1, dec2) -> ()) (curry (\(((p1,p2,pType1,pType2), dec1, dec2), ()) -> ((p1,p2,pType1,pType2), dec1, dec2)))))

-------------------------------
-- 4. Organizing the continuation games

-- auxiliary functions organizing information
whoHasPermits1 :: Permit -> Permit -> Permit -> Permit -> Either (Permit,Permit,Permit,Permit) (Permit,Permit,Permit,Permit)
whoHasPermits1 p1 p2 p1Type p2Type = if p1 == 0 then Left (p1,p2,p1Type,p2Type)
                                                else Right (p1,p2,p1Type,p2Type)

whoHasPermits2 :: Permit -> Permit -> Permit -> Permit -> Either (Permit,Permit,Permit,Permit) (Permit,Permit,Permit,Permit)
whoHasPermits2 p1 p2 p1Type p2Type= if p1 == 1 then Left (p1,p2,p1Type,p2Type)
                                                else Right (p1,p2,p1Type,p2Type)


-- Nested assigning of continuation games
whichCont1Src =  Block ["p1","p2","pType1","pType2"] []
                [Line ["whoHasPermits1 p1 p2 pType1 pType2"] [] "productionDec02 +++ productionCont2"   ["discard"] []]
                  [] []

whichCont1 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, pType1, pType2, discard) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), whoHasPermits1 p1 p2 pType1 pType2)) (\((p1, p2, pType1, pType2, discard), ()) -> (p1, p2, pType1, pType2, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec02 +++ productionCont2)))))) >>> (fromFunctions (\((p1, p2, pType1, pType2), discard) -> (p1, p2, pType1, pType2, discard)) (\(p1, p2, pType1, pType2, discard) -> ((p1, p2, pType1, pType2, discard), ())))))))) >>> (fromLens (\(p1, p2, pType1, pType2, discard) -> ()) (curry (\((p1, p2, pType1, pType2, discard), ()) -> (p1, p2, pType1, pType2, discard)))))

productionCont2Src = Block ["(p1,p2,pType1,pType2)"] []
                    [Line ["whoHasPermits2 p1 p2 pType1 pType2"] [] "productionDec11 +++ productionDec20"   ["discard"] []]
                      [] []

productionCont2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, pType1, pType2, discard) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), whoHasPermits2 p1 p2 pType1 pType2)) (\((p1, p2, pType1, pType2, discard), ()) -> (p1, p2, pType1, pType2, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec11 +++ productionDec20)))))) >>> (fromFunctions (\((p1, p2, pType1, pType2), discard) -> (p1, p2, pType1, pType2, discard)) (\(p1, p2, pType1, pType2, discard) -> ((p1, p2, pType1, pType2, discard), ())))))))) >>> (fromLens (\(p1, p2, pType1, pType2, discard) -> ()) (curry (\((p1, p2, pType1, pType2, discard), ()) -> (p1, p2, pType1, pType2, discard)))))

-------------------------------
-- 4. Resale market


------------------------------
-- 5. Complete game
-- For now excluding a resale market

completeGameSrc = Block [] []
                [Line [] []                            "randomAllocation"   ["p1","p2","pType1","pType2"] [],
                 Line ["p1","p2","pType1","pType2"] [] "whichCont1"         []                            []]
                  [] []

completeGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, pType1, pType2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((randomAllocation)))))) >>> (fromFunctions (\((), (p1, p2, pType1, pType2)) -> (p1, p2, pType1, pType2)) (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), (p1, p2, pType1, pType2))) (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((whichCont1)))))) >>> (fromFunctions (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2)) (\(p1, p2, pType1, pType2) -> ((p1, p2, pType1, pType2), ()))))))))) >>> (fromLens (\(p1, p2, pType1, pType2) -> ()) (curry (\((p1, p2, pType1, pType2), ()) -> (p1, p2, pType1, pType2)))))



------------------------------
-- 6. Analysis
eqGame =  equilibrium completeGame trivialContext


strategyPermit (recP,reqP) | recP < reqP         = certainly 0
                     -- ^ If not sufficient permits received produce nothing
                     | recP >= reqP && reqP == 1 = certainly 1
                     -- ^ If sufficient permits received and efficient production, use permits
                     | otherwise                 = certainly 0
                     -- ^ If sufficient permits received but inefficient production, do not use permits.

-- eqGame (((),(),()),((strategyPermit,strategyPermit),((strategyPermit,strategyPermit),(strategyPermit,strategyPermit))))

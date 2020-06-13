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
profit :: Value -> Permit -> Permit -> Permit -> Cost -> Profit
profit v reqP receivedP usedP c | receivedP < reqP                  =   - c*usedP
                                | receivedP >= reqP && usedP < reqP =   - c*usedP
                                | otherwise                         = v - c*usedP

-- companies no of permits needed
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
-- 3. Defining continuation games for pro 
-- random allocation of permits




-------------------------------
-- 4. Production continuation games
-- Without feeding information forward
productionDecSrc02 = Block ["p1","p2","pType1","pType2"] []
                    [Line ["p1"] [] "decision \"player1\" [0]"     ["dec1"] ["profit value pType1 p1 dec1 cost"],
                     Line ["p2"] [] "decision \"player2\" [0,1,2]" ["dec2"] ["profit value pType2 p2 dec2 cost"]]
                     [] []


productionDecSrc11 = Block ["p1","p2","pType1","pType2"] []
                    [Line ["p1"] [] "decision \"player1\" [0,1]" ["dec1"] ["profit value pType1 p1 dec1 cost"],
                     Line ["p2"] [] "decision \"player2\" [0,1]" ["dec2"] ["profit value pType2 p2 dec2 cost"]]
                     [] []

productionDecSrc20 = Block ["p1","p2","pType1","pType2"] []
                    [Line ["p1"] [] "decision \"player1\" [0,1,2]" ["dec1"] ["profit value pType1 p1 dec1 cost"],
                     Line ["p2"] [] "decision \"player2\" [0]"     ["dec2"] ["profit value pType2 p2 dec2 cost"]]
                     [] []
--productionDec = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, p3, pType1, pType2, pType3) -> ((p1, p2, p3, pType1, pType2, pType3), p1)) (\((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), ()) -> (p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player1" [0..p1])))))) >>> (fromFunctions (\((p1, p2, p3, pType1, pType2, pType3), dec1) -> (p1, p2, p3, pType1, pType2, pType3, dec1)) (\(p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3) -> ((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), profit value pType1 p1 dec1 cost))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, p3, pType1, pType2, pType3, dec1) -> ((p1, p2, p3, pType1, pType2, pType3, dec1), p2)) (\((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), ()) -> (p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player2" [0..p2])))))) >>> (fromFunctions (\((p1, p2, p3, pType1, pType2, pType3, dec1), dec2) -> (p1, p2, p3, pType1, pType2, pType3, dec1, dec2)) (\(p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3) -> ((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), profit value pType2 p2 dec2 cost)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, p3, pType1, pType2, pType3, dec1, dec2) -> ((p1, p2, p3, pType1, pType2, pType3, dec1, dec2), p3)) (\((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), ()) -> (p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "player3" [0..p3])))))) >>> (fromFunctions (\((p1, p2, p3, pType1, pType2, pType3, dec1, dec2), dec3) -> (p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3)) (\(p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3) -> ((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), profit value pType3 p3 dec3 cost))))))))) >>> (fromLens (\(p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3) -> ()) (curry (\((p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3), ()) -> (p1, p2, p3, pType1, pType2, pType3, dec1, dec2, dec3)))))


-------------------------------
-- 4. Resale market


------------------------------
-- 5. Complete game


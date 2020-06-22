{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.PermitSale.PermitSale where

import qualified Control.Arrow as CA
import           Numeric.Probability.Distribution
--import           OpenGames.Engine.BayesianDiagnostics
import qualified OpenGames.Engine.StatefulPayoffs as SP
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
profit :: Value -> Cost -> Permit -> Profit
profit v c p =  (v - c)*p

-- companies: number of permits needed for production
prior :: Stochastic Value
prior =  uniform [1..maximalValue]


--------------------------------
-- 1. Concrete parameterizations
cost :: Cost
cost  = 3

availablePermits :: Permit
availablePermits = 2

maximalValue :: Value
maximalValue = 5

randomAll :: Permit -> Stochastic (Permit,Permit)
randomAll noP = do
       p1 <- uniform [0..noP]
       p2 <- certainly (noP - p1)
       return (p1,p2)

-------------------------------
-- 2. Nature determining types
natureStageSrc = Block [] []
            [Line [] [] "nature prior" ["v1"] [],
             Line [] [] "nature prior" ["v2"] []]
            ["v1","v2"] []

natureStage = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2), ()) -> (v1, v2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature prior)))))) >>> (fromFunctions (\((), v1) -> v1) (\(v1, v2) -> ((v1, v2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\v1 -> (v1, ())) (\((v1, v2), ()) -> (v1, v2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature prior)))))) >>> (fromFunctions (\(v1, v2) -> (v1, v2)) (\(v1, v2) -> ((v1, v2), ()))))))))) >>> (fromLens (\(v1, v2) -> (v1, v2)) (curry (\((v1, v2), ()) -> (v1, v2)))))

-------------------------------
-- 3. Initial allocation phase
-- 3.0 random allocation of permits
randomAllocationSrc = Block [] []
             [Line [] [] "nature (randomAll availablePermits)" ["p1", "p2"] []]
             ["p1","p2"] []

randomAllocation = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((p1, p2), ()) -> (p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (randomAll availablePermits))))))) >>> (fromFunctions (\((), (p1, p2)) -> (p1, p2)) (\(p1, p2) -> ((p1, p2), ())))))))) >>> (fromLens (\(p1, p2) -> (p1, p2)) (curry (\((p1, p2), ()) -> (p1, p2)))))



-- 3.1 allocation of permits through fixed price
exogenousPriceSrc  = Block ["v1 :: Value","v2 :: Value"] []
             [Line ["\"player1\"", "[0..availablePermits]", "v1"] [] "dependentDecision" ["ask1"] ["- cost* p1"],
              Line ["\"player2\"", "[0..availablePermits]", "v2"] [] "dependentDecision" ["ask2"] ["- cost* p2"],
              Line ["(ask1,ask2)"] []    "fromFunctions allocatePermits id"  ["(p1,p2)"] []]
             ["p1","p2"] []


exogenousPrice = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value) -> ((v1 :: Value, v2 :: Value), ("player1", [0..availablePermits], v1))) (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value), ask1) -> (v1 :: Value, v2 :: Value, ask1)) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), - cost* p1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value, ask1) -> ((v1 :: Value, v2 :: Value, ask1), ("player2", [0..availablePermits], v2))) (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value, ask1), ask2) -> (v1 :: Value, v2 :: Value, ask1, ask2)) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), - cost* p2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value, ask1, ask2) -> ((v1 :: Value, v2 :: Value, ask1, ask2), (ask1,ask2))) (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions allocatePermits id)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value, ask1, ask2), (p1,p2)) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2))) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()))))))))) >>> (fromLens (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> (p1, p2)) (curry (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2))))))


-- NOTE change later to account better for overdemand
allocatePermits :: (Permit,Permit) -> (Permit,Permit)
allocatePermits  (ask1,ask2)   | ask1 + ask2 <= 2 = (ask1,ask2)
                               | otherwise        = (1,1)


-- 3.2 VCG auction of permits
-- NOTE Simplification of bidding function due to constant MV; needs to be adapted in case of decreasing demand
vcgSrc =  Block ["v1 :: Value","v2 :: Value"] []
          [Line ["\"player1\"", "[0..(2*maximalValue)]", "v1"] [] "dependentDecision" ["bid1"] ["- b1"],
           Line ["\"player2\"", "[0..(2*maximalValue)]", "v2"] [] "dependentDecision" ["bid2"] ["- b2"],
           Line ["(bid1,bid2)"] []    "fromFunctions vcgAllocation id"  ["((p1,b1),(p2,b2))"] []]
          ["p1","p2"] []

vcg = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value) -> ((v1 :: Value, v2 :: Value), ("player1", [0..(2*maximalValue)], v1))) (\((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), ()) -> (v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value), bid1) -> (v1 :: Value, v2 :: Value, bid1)) (\(v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))) -> ((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), - b1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value, bid1) -> ((v1 :: Value, v2 :: Value, bid1), ("player2", [0..(2*maximalValue)], v2))) (\((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), ()) -> (v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value, bid1), bid2) -> (v1 :: Value, v2 :: Value, bid1, bid2)) (\(v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))) -> ((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), - b2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value, bid1, bid2) -> ((v1 :: Value, v2 :: Value, bid1, bid2), (bid1,bid2))) (\((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), ()) -> (v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions vcgAllocation id)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value, bid1, bid2), ((p1,b1),(p2,b2))) -> (v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2)))) (\(v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))) -> ((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), ()))))))))) >>> (fromLens (\(v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))) -> (p1, p2)) (curry (\((v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2))), ()) -> (v1 :: Value, v2 :: Value, bid1, bid2, ((p1,b1),(p2,b2)))))))

-- NOTE simplification of allocation rule due to two players
vcgAllocation :: (Value,Value) -> ((Permit,Cost),(Permit,Cost))
vcgAllocation (b1,b2) | b1 == b2 = ((1,b2/2),(1,b1/2))
                      | b1 >  b2 = ((2,b2),(0,0))
                      | b1 <  b2 = ((0,0),(2,b1))

-------------------------------
-- 4. Production continuation games
-- Without feeding information forward

productionDecSrc = Block ["p1", "p2", "v1", "v2"] []
  [Line ["\"player1\"", "[0 .. p1]", "(p1, v1)"] [] "dependentDecision" ["dec1"] ["profit v1 0 dec1"],
   Line ["\"player2\"", "[0 .. p2]", "(p2, v2)"] [] "dependentDecision" ["dec2"] ["profit v2 0 dec2"]]
  [] []

productionDec = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, v1, v2, dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, v1, v2) -> ((p1, p2, v1, v2), ("player1", [0 .. p1], (p1, v1)))) (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, v1, v2), dec1) -> (p1, p2, v1, v2, dec1)) (\(p1, p2, v1, v2, dec1, dec2) -> ((p1, p2, v1, v2, dec1, dec2), profit v1 0 dec1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, v1, v2, dec1) -> ((p1, p2, v1, v2, dec1), ("player2", [0 .. p2], (p2, v2)))) (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, v1, v2, dec1), dec2) -> (p1, p2, v1, v2, dec1, dec2)) (\(p1, p2, v1, v2, dec1, dec2) -> ((p1, p2, v1, v2, dec1, dec2), profit v2 0 dec2))))))))) >>> (fromLens (\(p1, p2, v1, v2, dec1, dec2) -> ()) (curry (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2)))))

-------------------------------
-- 5. Resale market


------------------------------
-- 6. Complete game
-- For now excluding a resale market

-- 6.0 With initial random allocation
completeGameSrc = Block [] []
  [Line [] [] "natureStage"      ["v1","v2"] [],
   Line [] [] "randomAllocation" ["p1","p2"] [],
   Line ["p1","p2","v1","v2"] [] "productionDec" [] []]
  [] []

completeGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2, p1, p2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((natureStage)))))) >>> (fromFunctions (\((), (v1, v2)) -> (v1, v2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2) -> ((v1, v2), ())) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((randomAllocation)))))) >>> (fromFunctions (\((v1, v2), (p1, p2)) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), (p1, p2, v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec)))))) >>> (fromFunctions (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))))))) >>> (fromLens (\(v1, v2, p1, p2) -> ()) (curry (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)))))


-- 6.1 With fixed price allocation
completeGameEPSrc = Block [] []
  [Line [] [] "natureStage"             ["v1","v2"] [],
   Line ["v1","v2"] [] "exogenousPrice" ["p1","p2"] [],
   Line ["p1","p2","v1","v2"] [] "productionDec" [] []]
  [] []

completeGameEP = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2, p1, p2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((natureStage)))))) >>> (fromFunctions (\((), (v1, v2)) -> (v1, v2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2) -> ((v1, v2), (v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((exogenousPrice)))))) >>> (fromFunctions (\((v1, v2), (p1, p2)) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), (p1, p2, v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec)))))) >>> (fromFunctions (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))))))) >>> (fromLens (\(v1, v2, p1, p2) -> ()) (curry (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)))))

-- 6.1 With fixed price allocation
completeGameVCGSrc = Block [] []
  [Line [] [] "natureStage"             ["v1","v2"] [],
   Line ["v1","v2"] [] "vcg" ["p1","p2"] [],
   Line ["p1","p2","v1","v2"] [] "productionDec" [] []]
  [] []

completeGameVCG = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2, p1, p2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((natureStage)))))) >>> (fromFunctions (\((), (v1, v2)) -> (v1, v2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2) -> ((v1, v2), (v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((vcg)))))) >>> (fromFunctions (\((v1, v2), (p1, p2)) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), (p1, p2, v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec)))))) >>> (fromFunctions (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))))))) >>> (fromLens (\(v1, v2, p1, p2) -> ()) (curry (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)))))


------------------------------
-- 6. Analysis


eqGame =  equilibrium completeGame void

eqGameEP = equilibrium completeGameEP void

eqGameVCG = equilibrium completeGameVCG void

strProdStcopy (recP,_) = certainly recP

strInitAlloStAll v = if v >= cost then certainly availablePermits
                            else certainly 0

strInitAlloStConst v = certainly v

strInitAlloStVCG v = certainly (2*v)

-- eq.
test = eqGame (((),()),(),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
test2 = eqGameEP (((),()),(CA.Kleisli strInitAlloStAll, CA.Kleisli strInitAlloStAll,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
test3 = eqGameVCG (((),()),(CA.Kleisli strInitAlloStVCG, CA.Kleisli strInitAlloStVCG,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))

-- non eq.
test4 = eqGameEP (((),()),(CA.Kleisli strInitAlloStConst, CA.Kleisli strInitAlloStAll,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
test5 = eqGameVCG (((),()),(CA.Kleisli strInitAlloStAll, CA.Kleisli strInitAlloStAll,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))

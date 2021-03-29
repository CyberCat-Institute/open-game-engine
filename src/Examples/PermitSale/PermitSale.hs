{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Examples.PermitSale.PermitSale where

import qualified Control.Arrow as CA
import           Numeric.Probability.Distribution
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

data Role = Seller | Buyer | NoRole
   deriving (Eq,Ord,Show)


-- companies payoffs
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


randomAll ::  Stochastic (Permit,Permit)
randomAll = uniform [(2,0),(1,1),(0,1)]

randomAll2 :: Permit -> Stochastic (Permit,Permit)
randomAll2 noP = do
       p1 <- uniform [0..noP]
       p2 <- uniform [(noP - p1)..noP]
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
             [Line [] [] "nature randomAll" ["p1", "p2"] []]
             ["p1","p2"] []

randomAllocation = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2) -> ())) >>> (reindex (\a1 -> a1) (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((p1, p2), ()) -> (p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature randomAll)))))) >>> (fromFunctions (\((), (p1, p2)) -> (p1, p2)) (\(p1, p2) -> ((p1, p2), ())))))))) >>> (fromLens (\(p1, p2) -> (p1, p2)) (curry (\((p1, p2), ()) -> (p1, p2)))))



-- 3.1 allocation of permits through fixed price
exogenousPriceSrc  = Block ["v1 :: Value","v2 :: Value"] []
             [Line ["\"player1\"", "[0..availablePermits]", "v1"] [] "dependentDecision" ["ask1"] ["- cost* p1"],
              Line ["\"player2\"", "[0..availablePermits]", "v2"] [] "dependentDecision" ["ask2"] ["- cost* p2"],
              Line ["(ask1,ask2)"] []    "fromFunctions allocatePermits id"  ["(p1,p2)"] []]
             ["p1","p2"] []


exogenousPrice = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value) -> ((v1 :: Value, v2 :: Value), ("player1", [0..availablePermits], v1))) (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value), ask1) -> (v1 :: Value, v2 :: Value, ask1)) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), - cost* p1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value, ask1) -> ((v1 :: Value, v2 :: Value, ask1), ("player2", [0..availablePermits], v2))) (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value, ask1), ask2) -> (v1 :: Value, v2 :: Value, ask1, ask2)) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), - cost* p2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1 :: Value, v2 :: Value, ask1, ask2) -> ((v1 :: Value, v2 :: Value, ask1, ask2), (ask1,ask2))) (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions allocatePermits id)))))) >>> (fromFunctions (\((v1 :: Value, v2 :: Value, ask1, ask2), (p1,p2)) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2))) (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> ((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()))))))))) >>> (fromLens (\(v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)) -> (p1, p2)) (curry (\((v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2)), ()) -> (v1 :: Value, v2 :: Value, ask1, ask2, (p1,p2))))))


-- NOTE change later to account better for overdemand
allocatePermits :: (Permit,Permit) -> (Permit,Permit)
allocatePermits  (ask1,ask2)   | ask1 + ask2 <= 2       = (ask1,ask2)
                               | ask1 == 0 && ask2 >  2 = (0,2)
                               | ask1 >  2 && ask2 == 0 = (2,0)
                               | otherwise              = (1,1)


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
-- 4.0. Without feeding information forward

-- TODO fix the dependent decision - delivers wrong result
productionDecSrc = Block ["p1", "p2", "v1", "v2"] []
  [Line ["\"player1\"", "[0 .. p1]", "(p1, v1)"] [] "dependentDecision" ["dec1"] ["profit v1 0 dec1"],
   Line ["\"player2\"", "[0 .. p2]", "(p2, v2)"] [] "dependentDecision" ["dec2"] ["profit v2 0 dec2"]]
  [] []

productionDec = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, v1, v2, dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, v1, v2) -> ((p1, p2, v1, v2), ("player1", [0 .. p1], (p1, v1)))) (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, v1, v2), dec1) -> (p1, p2, v1, v2, dec1)) (\(p1, p2, v1, v2, dec1, dec2) -> ((p1, p2, v1, v2, dec1, dec2), profit v1 0 dec1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, v1, v2, dec1) -> ((p1, p2, v1, v2, dec1), ("player2", [0 .. p2], (p2, v2)))) (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, v1, v2, dec1), dec2) -> (p1, p2, v1, v2, dec1, dec2)) (\(p1, p2, v1, v2, dec1, dec2) -> ((p1, p2, v1, v2, dec1, dec2), profit v2 0 dec2))))))))) >>> (fromLens (\(p1, p2, v1, v2, dec1, dec2) -> ()) (curry (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2)))))

-- 4.1 With feeding information forward
-- TODO fix the dependent decision - delivers wrong result
productionDecContSrc = Block ["p1", "p2", "v1", "v2"] []
  [Line ["\"player1\"", "[0 .. p1]", "(p1, v1)"] [] "dependentDecision" ["dec1"] ["profit v1 0 dec1"],
   Line ["\"player2\"", "[0 .. p2]", "(p2, v2)"] [] "dependentDecision" ["dec2"] ["profit v2 0 dec2"]]
  ["p1-dec1","p2 - dec2"] []

productionDecCont = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(p1, p2, v1, v2, dec1, dec2) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, v1, v2) -> ((p1, p2, v1, v2), ("player1", [0 .. p1], (p1, v1)))) (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, v1, v2), dec1) -> (p1, p2, v1, v2, dec1)) (\(p1, p2, v1, v2, dec1, dec2) -> ((p1, p2, v1, v2, dec1, dec2), profit v1 0 dec1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(p1, p2, v1, v2, dec1) -> ((p1, p2, v1, v2, dec1), ("player2", [0 .. p2], (p2, v2)))) (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((p1, p2, v1, v2, dec1), dec2) -> (p1, p2, v1, v2, dec1, dec2)) (\(p1, p2, v1, v2, dec1, dec2) -> ((p1, p2, v1, v2, dec1, dec2), profit v2 0 dec2))))))))) >>> (fromLens (\(p1, p2, v1, v2, dec1, dec2) -> (p1-dec1, p2 - dec2)) (curry (\((p1, p2, v1, v2, dec1, dec2), ()) -> (p1, p2, v1, v2, dec1, dec2)))))

-------------------------------
-- 5. Resale market

--[Line ["whichSeller (ContResale player1Type player2Type signalPlayer1 signalPlayer2 dec1 dec2)"] ["price1","price2"] "resaleSeller1 +++ resaleSeller2"   ["discard"] []]



resaleStageSrc = Block ["r1","r2","v1 :: Value","v2 :: Value"] []
                     [Line ["\"player1\"", "[0..maximalValue]", "(fst (whoIsSeller r1 r2 v1 v2))"] [] "dependentDecision" ["price1"] ["t1"],
                      Line ["\"player2\"", "[0..maximalValue]", "(snd (whoIsSeller r1 r2 v1 v2))"] [] "dependentDecision" ["price2"] ["t2"],
                      Line ["(price1,price2,r1,r2)"] []    "fromFunctions resaleMarketClearing id"  ["((p1,t1),(p2,t2))"] []]
                      ["p1","p2"] []


resaleStage = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(r1, r2, v1 :: Value, v2 :: Value) -> ((r1, r2, v1 :: Value, v2 :: Value), ("player1", [0..maximalValue], (fst (whoIsSeller r1 r2 v1 v2))))) (\((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), ()) -> (r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((r1, r2, v1 :: Value, v2 :: Value), price1) -> (r1, r2, v1 :: Value, v2 :: Value, price1)) (\(r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))) -> ((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), t1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(r1, r2, v1 :: Value, v2 :: Value, price1) -> ((r1, r2, v1 :: Value, v2 :: Value, price1), ("player2", [0..maximalValue], (snd (whoIsSeller r1 r2 v1 v2))))) (\((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), ()) -> (r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision)))))) >>> (fromFunctions (\((r1, r2, v1 :: Value, v2 :: Value, price1), price2) -> (r1, r2, v1 :: Value, v2 :: Value, price1, price2)) (\(r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))) -> ((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), t2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(r1, r2, v1 :: Value, v2 :: Value, price1, price2) -> ((r1, r2, v1 :: Value, v2 :: Value, price1, price2), (price1,price2,r1,r2))) (\((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), ()) -> (r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((fromFunctions resaleMarketClearing id)))))) >>> (fromFunctions (\((r1, r2, v1 :: Value, v2 :: Value, price1, price2), ((p1,t1),(p2,t2))) -> (r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2)))) (\(r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))) -> ((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), ()))))))))) >>> (fromLens (\(r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))) -> (p1, p2)) (curry (\((r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2))), ()) -> (r1, r2, v1 :: Value, v2 :: Value, price1, price2, ((p1,t1),(p2,t2)))))))

whoIsSeller :: Permit -> Permit -> Value -> Value -> ((Role,Value),(Role,Value))
whoIsSeller prmt1 prmt2 v1 v2 |prmt1 > prmt2  = ((Seller,v1),(Buyer,v2))
                              |prmt2 > prmt1  = ((Buyer,v1),(Seller,v2))
                              | otherwise = ((NoRole,v1),(NoRole,v2))

-- NOTE Resale restricted to one unit here. Need for generalized continuation game
resaleMarketClearing :: (Cost,Cost,Permit,Permit) -> ((Permit,Cost),(Permit,Cost))
resaleMarketClearing (p1,p2,prmt1,prmt2) | prmt1 > prmt2 && p2 > p1 =  ((prmt1-1,pFinal),(prmt2 + 1,-pFinal))
                                         | prmt2 > prmt1 && p1 > p2 =  ((prmt1 + 1,-pFinal),(prmt2 - 1,pFinal))
                                         | otherwise             =   ((prmt1,0),(prmt2,0))
   where pFinal = (p1 + p2)/2

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

-- 6.2 With initial auction stage
completeGameVCGSrc = Block [] []
  [Line [] [] "natureStage"             ["v1","v2"] [],
   Line ["v1","v2"] [] "vcg" ["p1","p2"] [],
   Line ["p1","p2","v1","v2"] [] "productionDec" [] []]
  [] []

completeGameVCG = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2, p1, p2) -> ())) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((natureStage)))))) >>> (fromFunctions (\((), (v1, v2)) -> (v1, v2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2) -> ((v1, v2), (v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((vcg)))))) >>> (fromFunctions (\((v1, v2), (p1, p2)) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), (p1, p2, v1, v2))) (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec)))))) >>> (fromFunctions (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), ()))))))))) >>> (fromLens (\(v1, v2, p1, p2) -> ()) (curry (\((v1, v2, p1, p2), ()) -> (v1, v2, p1, p2)))))



-- 6.3 Random allocation with resale and one production stage
completeGameResaleSrc = Block [] []
  [Line [] [] "natureStage"             ["v1","v2"] [],
   Line [] [] "randomAllocation" ["p1","p2"] [],
   Line ["p1","p2","v1","v2"] [] "resaleStage"   ["r1","r2"] [],
   Line ["r1","r2","v1","v2"] [] "productionDec" [] []]
  [] []

completeGameResale = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2, p1, p2, r1, r2) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2, p1, p2, r1, r2), ()) -> (v1, v2, p1, p2, r1, r2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((natureStage)))))) >>> (fromFunctions (\((), (v1, v2)) -> (v1, v2)) (\(v1, v2, p1, p2, r1, r2) -> ((v1, v2, p1, p2, r1, r2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2) -> ((v1, v2), ())) (\((v1, v2, p1, p2, r1, r2), ()) -> (v1, v2, p1, p2, r1, r2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((randomAllocation)))))) >>> (fromFunctions (\((v1, v2), (p1, p2)) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2, r1, r2) -> ((v1, v2, p1, p2, r1, r2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), (p1, p2, v1, v2))) (\((v1, v2, p1, p2, r1, r2), ()) -> (v1, v2, p1, p2, r1, r2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((resaleStage)))))) >>> (fromFunctions (\((v1, v2, p1, p2), (r1, r2)) -> (v1, v2, p1, p2, r1, r2)) (\(v1, v2, p1, p2, r1, r2) -> ((v1, v2, p1, p2, r1, r2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2, r1, r2) -> ((v1, v2, p1, p2, r1, r2), (r1, r2, v1, v2))) (\((v1, v2, p1, p2, r1, r2), ()) -> (v1, v2, p1, p2, r1, r2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDec)))))) >>> (fromFunctions (\((v1, v2, p1, p2, r1, r2), ()) -> (v1, v2, p1, p2, r1, r2)) (\(v1, v2, p1, p2, r1, r2) -> ((v1, v2, p1, p2, r1, r2), ()))))))))) >>> (fromLens (\(v1, v2, p1, p2, r1, r2) -> ()) (curry (\((v1, v2, p1, p2, r1, r2), ()) -> (v1, v2, p1, p2, r1, r2)))))

-- 6.4 Random allocation with resale and two production stages
completeGameResale2Src = Block [] []
  [Line [] [] "natureStage"             ["v1","v2"] [],
   Line [] [] "randomAllocation" ["p1","p2"] [],
   Line ["p1","p2","v1","v2"] [] "productionDecCont" ["r1","r2"] [],
   Line ["r1","r2","v1","v2"] [] "resaleStage"       ["l1","l2"] [],
   Line ["l1","l2","v1","v2"] [] "productionDecCont" ["discard1","discard2"][]]
  [] []

completeGameResale2 = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ())) >>> (reindex (\(a1, a2, a3, a4, a5) -> ((((a1, a2), a3), a4), a5)) (((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((natureStage)))))) >>> (fromFunctions (\((), (v1, v2)) -> (v1, v2)) (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2) -> ((v1, v2), ())) (\((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((randomAllocation)))))) >>> (fromFunctions (\((v1, v2), (p1, p2)) -> (v1, v2, p1, p2)) (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2) -> ((v1, v2, p1, p2), (p1, p2, v1, v2))) (\((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDecCont)))))) >>> (fromFunctions (\((v1, v2, p1, p2), (r1, r2)) -> (v1, v2, p1, p2, r1, r2)) (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2, r1, r2) -> ((v1, v2, p1, p2, r1, r2), (r1, r2, v1, v2))) (\((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((resaleStage)))))) >>> (fromFunctions (\((v1, v2, p1, p2, r1, r2), (l1, l2)) -> (v1, v2, p1, p2, r1, r2, l1, l2)) (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(v1, v2, p1, p2, r1, r2, l1, l2) -> ((v1, v2, p1, p2, r1, r2, l1, l2), (l1, l2, v1, v2))) (\((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((productionDecCont)))))) >>> (fromFunctions (\((v1, v2, p1, p2, r1, r2, l1, l2), (discard1, discard2)) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2)) (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()))))))))) >>> (fromLens (\(v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2) -> ()) (curry (\((v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2), ()) -> (v1, v2, p1, p2, r1, r2, l1, l2, discard1, discard2)))))

------------------------------
-- 6. Analysis


eqGame =  equilibrium completeGame void

eqGameEP = equilibrium completeGameEP void

eqGameVCG = equilibrium completeGameVCG void

eqGameRes = equilibrium completeGameResale void

eqGameRes2 = equilibrium completeGameResale2 void

strProdStcopy :: Num prob => (Double,Double) -> T prob Double
strProdStcopy (recP,_) = certainly recP

strProdStcont (recP,_) = if recP == 2 then certainly 1 else certainly recP

strInitAlloStAll v = if v >= cost then certainly availablePermits
                                  else certainly 0

strInitAlloStConst v = certainly v

strInitAlloStVCG v = certainly (2*v)

strResAlloSt :: Num prob => (Role,Value) -> T prob Value
strResAlloSt (NoRole,_) = certainly 0
strResAlloSt (Buyer,1)  = certainly 1
strResAlloSt (Buyer,2)  = certainly 2
strResAlloSt (Buyer,3)  = certainly 2
strResAlloSt (Buyer,4)  = certainly 4
strResAlloSt (Buyer,5)  = certainly 4
strResAlloSt (Seller,1) = certainly 3
strResAlloSt (Seller,2) = certainly 3
strResAlloSt (Seller,3) = certainly 3
strResAlloSt (Seller,4) = certainly 5
strResAlloSt (Seller,5) = certainly 5

-- eq.
test = eqGame (((),()),(),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
test2 = eqGameEP (((),()),(CA.Kleisli strInitAlloStAll, CA.Kleisli strInitAlloStAll,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
test3 = eqGameVCG (((),()),(CA.Kleisli strInitAlloStVCG, CA.Kleisli strInitAlloStVCG,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
test4 = eqGameRes (((),()), (),(CA.Kleisli strResAlloSt, CA.Kleisli strResAlloSt,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))


-- non eq.
testA = eqGameEP (((),()),(CA.Kleisli strInitAlloStConst, CA.Kleisli strInitAlloStAll,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
testB = eqGameVCG (((),()),(CA.Kleisli strInitAlloStAll, CA.Kleisli strInitAlloStAll,()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
testC = eqGameRes2 (((),()),(),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy),(CA.Kleisli strResAlloSt, CA.Kleisli strResAlloSt, ()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))
testD = eqGameRes2 (((),()),(),(CA.Kleisli strProdStcont,CA.Kleisli strProdStcont),(CA.Kleisli strResAlloSt, CA.Kleisli strResAlloSt, ()),(CA.Kleisli strProdStcopy,CA.Kleisli strProdStcopy))

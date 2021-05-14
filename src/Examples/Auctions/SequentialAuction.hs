{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Examples.Auctions.SequentialAuction where

import Control.Arrow (Kleisli(..))
import Data.List
import Language.Haskell.TH
import System.Console.Haskeline
import Text.Read

import Engine.Engine
import Preprocessor.Preprocessor




-- TODO Generalize to population of players

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double

values = [20,30,60]

reservePrice = 1

----------------------
-- 1 Preliminary stuff

-- Order bids from large to small
orderAllocation :: [(String, Values)] -> [(String, Values)]
orderAllocation  = sortBy (flip (\(_,v1) (_,v2) -> compare v1 v2 ))

-- Determine k-max bid
kMaxBid :: Int -> [(String, Values)] ->  Values
kMaxBid k ls = snd $  orderAllocation ls !! (k-1)

-- k- price auction rule, i.e. the sequence for winning bidders is ignored, winners always pay k-highest price
noLotteryPayment :: Values -> Values -> Int -> Int -> Int -> [(String,Values,Bool)] -> [(String, Values)]
noLotteryPayment _        _    _         _             _               []                     = []
noLotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then (name,kmax) : noLotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : noLotteryPayment resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : noLotteryPayment resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls

-- Determine payments for winners; for lottery winners, and for those who do not get a good set it to 0
lotteryPayment :: Values -> Values -> Int -> Int -> Int -> [(String,Values,Bool)] -> [(String, Values)]
lotteryPayment _        _    _         _             _               []                     = []
lotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then
           if counterWinner < noLottery then (name,resPrice) : lotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
                                        else (name,kmax) : lotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : lotteryPayment resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : lotteryPayment resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls


-- Mark the auctionWinners 
auctionWinner :: Values -> [(String, Values)] -> [(String, Values,Bool)]
auctionWinner kmax []= []
auctionWinner kmax ls@((name,b):bs)=
  if b < kmax then (name,b,False) : auctionWinner kmax bs
              else (name,b,True)  : auctionWinner kmax bs

-- Select the payment for a player given the list of payments
selectPayoffs :: String -> [(String,Values)] -> Values
selectPayoffs name [] = 0
selectPayoffs name ((n,p):ls) = if name == n then p else selectPayoffs name ls

-- Determines the payoff for each player
setPayoff :: (String,Values) -> [(String, Values)] -> Values
setPayoff (name,value) payments =
  if pay == 0 then 0 else value - pay
  where
    pay =  selectPayoffs name payments



-- Determine the payments given k-highest price (1,2..) and no of winnerSlots being allocated through auction; and _noLotteries_ slots through lottery
-- NOTE the restriction of k-highest price and number of slots
auctionPayment :: (Values -> Values -> Int -> Int -> Int -> [(String,Values,Bool)] -> [(String, Values)])
                 -- ^ Payment function
                 -> Int -> Int -> Int -> [(String, Values)]
                 -- ^ Parameters
                 -> [(String, Values)]
auctionPayment paymentFunction kPrice winnerSlots noLotteries ls =
  if kMax > kThreshold
     then paymentFunction reservePrice kThreshold noLotteries 0 0 (auctionWinner kThreshold ls)
     else paymentFunction reservePrice kMax noLotteries 0 0 (auctionWinner kThreshold ls)
  where kMax = kMaxBid kPrice ls
        kThreshold = kMaxBid winnerSlots ls



-- Random shuffle of bids
shuffleBids :: [(String, Values)] -> Stochastic [(String, Values)]
shuffleBids ls = uniformDist $ permutations ls

---------------------
-- 1 The actual games

-- Draws a value and creates a pair of _value_ _name_
natureDrawsTypeStage name = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : value ;
    returns   :  ;
    :-----:

    outputs   :  (name,value) ;
    returns   :    ;
  |]

-- Individual bidding stage
biddingStage name = [opengame|

    inputs    :  nameValuePair  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  nameValuePair  ;
    feedback  :   ;
    operation :  dependentDecision name (const [0,20..60]) ;
    outputs   :  bid ;
    returns   :  setPayoff nameValuePair payments  ;
    :---------------------------:

    outputs   :  bid ;
    returns   :  payments  ;
  |]

-- Transforms the payments into a random reshuffling
transformPayments kPrice kSlots noLotteries paymentFunction = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : liftStochasticForward shuffleBids ;
   outputs   : shuffledBids ;
   returns   :      ;

   inputs    : shuffledBids ;
   feedback  :      ;
   operation : forwardFunction (auctionPayment paymentFunction kPrice kSlots noLotteries) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]



-- Instantiates a simplified version with three players
bidding3 kPrice kSlots noLotteries paymentFunction = [opengame|

   inputs    :      ;
   feedback  :      ;

   :-----------------:
   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Alice" ;
   outputs   :  aliceValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Bob" ;
   outputs   :  bobValue ;
   returns   :      ;

   inputs    :      ;
   feedback  :      ;
   operation : natureDrawsTypeStage "Carol" ;
   outputs   :  carolValue ;
   returns   :      ;

   inputs    :  aliceValue    ;
   feedback  :      ;
   operation :  biddingStage "Alice" ;
   outputs   :  aliceDec ;
   returns   :  payments  ;

   inputs    :  bobValue    ;
   feedback  :      ;
   operation :  biddingStage "Bob" ;
   outputs   :  bobDec ;
   returns   :  payments  ;

   inputs    :  carolValue    ;
   feedback  :      ;
   operation :  biddingStage "Carol" ;
   outputs   :  carolDec ;
   returns   :  payments  ;

   inputs    :  [("Alice",aliceDec),("Bob",bobDec),("Carol",carolDec)]  ;
   feedback  :      ;
   operation :   transformPayments kPrice kSlots noLotteries paymentFunction ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :      ;
   returns   :      ;
   |]

-- B Analysis
-------------

---------------
-- 0 Strategies

-- Truthful bidding
stratBidderTruth :: Kleisli Stochastic (String, Values) Values
stratBidderTruth  = Kleisli (\(_,x) -> playDeterministically x)

-- Bidding strategy with threshold 50 and value 10
stratBidderThreshold :: Kleisli Stochastic (String, Values) Values
stratBidderThreshold = Kleisli (\(_,x) -> if x >= 50 then playDeterministically 20 else playDeterministically 10)

-- Bidding with different threshold
stratBidderThreshold' :: Kleisli Stochastic (String, Values) Values
stratBidderThreshold' = Kleisli (\(_,x) -> case () of
                                              _ | x < 30    -> playDeterministically 0
                                                | x == 30   -> playDeterministically 10
                                                | otherwise -> playDeterministically 20)



-- Complete strategy for truthful bidding for 3 players
truthfulStrat ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
truthfulStrat =
  stratBidderTruth
  ::- stratBidderTruth
  ::- stratBidderTruth
  ::- Nil

-- Complete strategy for threshold bidding 3 players
thresholdStrat ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
thresholdStrat =
  stratBidderThreshold
  ::- stratBidderThreshold
  ::- stratBidderThreshold
  ::- Nil

-- Complete strategy for threshold' bidding 3 players
thresholdStrat' ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
thresholdStrat' =
  stratBidderThreshold'
  ::- stratBidderThreshold'
  ::- stratBidderThreshold'
  ::- Nil

---------------
-- 1 Equilibria
-- 1.0 Eq. game with 3 players
equilibriumGame kPrice kSlots noLotteries paymentFunction strat = evaluate (bidding3 kPrice kSlots noLotteries paymentFunction) strat void


------------------------
-- 2 Interactive session


-- 3 players with 1 auction slot, 2nd highest price, and 1 lottery slot - truthful bidding - not an eq
-- generateIsEq $ equilibriumGame 2 1 1 lotteryPayment truthfulStrat

-- 3 players with 1 auction slot, 2nd highest price is paid, and 1 lottery slot - threshold bidding - is an eq
-- generateIsEq $ equilibriumGame 2 1 1 lotteryPayment thresholdStrat

-- 3 players with 1 auction slot and 1 lottery slot AND 2nd price rule - strategies before not an equilibrium
-- generateIsEq $ equilibriumGame 2 1 1 noLotteryPayment thresholdStrat

-- Truthful bidding is also not an equilibrium
-- generateIsEq $ equilibriumGame 2 1 1 noLotteryPayment truthfulStrat

-- But the alternative threshold strategy is an equilibrium
-- generateIsEq $ equilibriumGame 2 1 1 noLotteryPayment thresholdStrat'

-- Also note: Once we exclude slots via lottery, and just auction off one slot, truthful bidding becomes an equilibrium
-- generateIsEq $ equilibriumGame 2 1 0 noLotteryPayment truthfulStrat



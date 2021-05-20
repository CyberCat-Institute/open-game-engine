module Examples.Auctions.AuctionSupportFunctions where

import Engine.Engine (Stochastic,uniformDist)
import Data.List
----------------------
-- 0. Types

----------------------
-- 1. Auction rules

-- Order bids from large to small
orderAllocation :: Ord v => [(n, v)] -> [(n, v)]
orderAllocation  = sortBy (flip (\(_,v1) (_,v2) -> compare v1 v2 ))

-- Determine k-max bid
kMaxBid :: Ord v => Int -> [(n, v)] ->  v
kMaxBid k ls = snd $  orderAllocation ls !! (k-1)

-- k- price auction rule, i.e. the sequence for winning bidders is ignored, winners always pay k-highest price
noLotteryPayment :: Num v => v -> v -> Int -> Int -> Int -> [(n,v,Bool)] -> [(n, v)]
noLotteryPayment _        _    _         _             _               []                     = []
noLotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then (name,kmax) : noLotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : noLotteryPayment resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : noLotteryPayment resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls

-- Determine payments for winners; for lottery winners, and for those who do not get a good set it to 0
lotteryPayment :: Num v => v -> v -> Int -> Int -> Int -> [(String,v,Bool)] -> [(String, v)]
lotteryPayment _        _    _         _             _               []                     = []
lotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then
           if counterWinner < noLottery then (name,resPrice) : lotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
                                        else (name,kmax) : lotteryPayment resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : lotteryPayment resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : lotteryPayment resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls


-- Determine payments for winners; for lottery winners, and for those who do not get a good set it to 0
-- Use a different payment rule; kmax is reduced by factor depending on number of lottery slots, pricing rule, and reserve price
lotteryPayment2 :: Fractional v => v -> v -> Int -> Int -> Int -> [(String,v,Bool)] -> [(String, v)]
lotteryPayment2 _        _    _         _             _               []                     = []
lotteryPayment2 resPrice kmax noLottery counterWinner lotteriesGiven ((name,bid,winner):ls)  =
   if winner
      then
           if counterWinner < noLottery then (name,resPrice) : lotteryPayment2 resPrice kmax noLottery counterWinner lotteriesGiven ls
                                        else (name,pay) : lotteryPayment2 resPrice kmax noLottery counterWinner lotteriesGiven ls
      else
           if noLottery > lotteriesGiven then (name,resPrice) : lotteryPayment2 resPrice kmax noLottery (counterWinner + 1) (lotteriesGiven + 1) ls
                                         else (name,0) : lotteryPayment2 resPrice kmax noLottery (counterWinner + 1) lotteriesGiven ls
   where pay = kmax  - (fromIntegral noLottery) / 2 * (kmax - resPrice)


-- Mark the auctionWinners 
auctionWinner :: Ord v => v -> [(n, v)] -> [(n, v,Bool)]
auctionWinner kmax []= []
auctionWinner kmax ls@((name,b):bs)=
  if b < kmax then (name,b,False) : auctionWinner kmax bs
              else (name,b,True)  : auctionWinner kmax bs

-- Select the payment for a player given the list of payments
selectPayoffs :: (Eq n, Num v) => n -> [(n,v)] -> v
selectPayoffs name [] = 0
selectPayoffs name ((n,p):ls) = if name == n then p else selectPayoffs name ls

-- Determines the payoff for each player
setPayoff :: (Eq n, Num v, Eq v) => (n,v) -> [(n, v)] -> v
setPayoff (name,value) payments =
  if pay == 0 then 0 else value - pay
  where
    pay =  selectPayoffs name payments



-- Determine the payments given k-highest price (1,2..) and no of winnerSlots being allocated through auction; and _noLotteries_ slots through lottery
-- NOTE the restriction of k-highest price and number of slots
auctionPayment :: Ord v => (v -> v -> Int -> Int -> Int -> [(n,v,Bool)] -> [(n, v)])
                 -- ^ Payment function
                 -> v
                 -- ^ Reserve Price
                 -> Int -> Int -> Int -> [(n, v)]
                 -- ^ Parameters
                 -> [(n, v)]
auctionPayment paymentFunction reservePrice kPrice winnerSlots noLotteries ls =
  if kMax > kThreshold
     then paymentFunction reservePrice kThreshold noLotteries 0 0 (auctionWinner kThreshold ls)
     else paymentFunction reservePrice kMax noLotteries 0 0 (auctionWinner kThreshold ls)
  where kMax = kMaxBid kPrice ls
        kThreshold = kMaxBid winnerSlots ls



-- Random shuffle of bids
shuffleBids :: [(n, v)] -> Stochastic [(n, v)]
shuffleBids ls = uniformDist $ permutations ls


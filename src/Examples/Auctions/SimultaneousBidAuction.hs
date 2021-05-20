{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Examples.Auctions.SimultaneousBidAuction where


import Engine.Engine
import Preprocessor.Preprocessor
import Examples.Auctions.AuctionSupportFunctions

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double 

values = [0,20..100]

reservePrice :: Double
reservePrice = 1

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
    operation :  dependentDecision name (const [0,20..100]) ;
    outputs   :  dec ;
    returns   :  setPayoff nameValuePair payments  ;
    :---------------------------:

    outputs   :  dec ;
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
   operation : forwardFunction (auctionPayment paymentFunction reservePrice kPrice kSlots noLotteries) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]



-- Instantiates a simplified version with three players
bidding2 kPrice kSlots noLotteries paymentFunction = [opengame| 

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

   inputs    :  [("Alice",aliceDec),("Bob",bobDec)]  ;
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



-- Complete strategy for truthful bidding for 2 players
truthfulStrat ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
truthfulStrat =
  stratBidderTruth
  ::- stratBidderTruth
  ::- Nil

---------------
-- 1 Equilibria
-- 1.0 Eq. game with 3 players
equilibriumGame kPrice kSlots noLotteries paymentFunction strat = evaluate (bidding2 kPrice kSlots noLotteries paymentFunction) strat void


------------------------
-- 2 Interactive session

-- One object being auctioned off Once we exclude slots via lottery, and just auction off one slot, truthful bidding becomes an equilibrium
-- generateIsEq $ equilibriumGame 2 1 0 noLotteryPayment truthfulStrat




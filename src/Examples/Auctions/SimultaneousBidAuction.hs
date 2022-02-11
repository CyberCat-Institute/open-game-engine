{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}


module Examples.Auctions.SimultaneousBidAuction where


import OpenGames
import OpenGames.Preprocessor
import Examples.Auctions.AuctionSupportFunctions

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double

values = [0,20..100]

reservePriceParameter :: Double
reservePriceParameter = 1

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
    outputs   :  bid ;
    returns   :  setPayoff nameValuePair payments  ;
    :---------------------------:

    outputs   :  bid ;
    returns   :  payments  ;
  |]


-- Transforms the payments into a random reshuffling
transformPaymentsReservePrice kPrice kSlots = [opengame|

   inputs    : (bids,reservePrice) ;
   feedback  :      ;

   :-----------------:
   inputs    : (bids,reservePrice) ;
   feedback  :      ;
   operation : forwardFunction (auctionPaymentResPrice noLotteryPayment kPrice kSlots 0) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]



bidding2ReservePrice kPrice kSlots = [opengame|

   inputs    : reservePrice    ;
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

   inputs    :  ([("Alice",aliceDec),("Bob",bobDec)],reservePrice)  ;
   feedback  :      ;
   operation :   transformPaymentsReservePrice kPrice kSlots ;
   outputs   :  payments ;
   returns   :      ;
   :-----------------:

   outputs   :  payments    ;
   returns   :      ;
   |]


---- Without reserve price
-- Transforms the payments into a random reshuffling
transformPayments kPrice kSlots reservePrice = [opengame|

   inputs    : bids ;
   feedback  :      ;

   :-----------------:
   inputs    : bids ;
   feedback  :      ;
   operation : forwardFunction (auctionPayment noLotteryPayment reservePrice kPrice kSlots 0) ;
   outputs   : payments ;
   returns   :      ;
   :-----------------:

   outputs   : payments ;
   returns   :      ;
  |]


-- Instantiates a simplified version with two players
bidding2 kPrice kSlots reservePrice  = [opengame|

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
   operation :   transformPayments kPrice kSlots reservePrice ;
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

-- Constant bidding
constBidding :: Values -> Kleisli Stochastic (String,Values) Values
constBidding x = Kleisli (\(_,_) -> playDeterministically x)

-- Complete strategy for truthful bidding for 2 players
truthfulStrat ::
  List
    '[Kleisli Stochastic (String, Values) Values,
      Kleisli Stochastic (String, Values) Values]
truthfulStrat =
  stratBidderTruth
  :- stratBidderTruth
  :- Nil

-- Complete strategy for const bidding for 2 players
constBiddingStrat x y =
  constBidding x
  :- constBidding y
  :- Nil

---------------
-- 1 Equilibria
-- 1.0 Eq. game with 3 players
equilibriumGame kPrice kSlots reservePrice strat = evaluate (bidding2 kPrice kSlots reservePrice) strat void


------------------------
-- 2 Interactive session

-- One object being auctioned off Once we exclude slots via lottery, and just auction off one slot, truthful bidding becomes an equilibrium
-- generateIsEq $ equilibriumGame 2 1 reservePriceParameter truthfulStrat

-- Not an equilibrium
-- generateIsEq $ equilibriumGame 2 1 reservePriceParameter (constBiddingStrat 30 30)


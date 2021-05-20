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
import Examples.Auctions.AuctionSupportFunctions



-- TODO Generalize to population of players

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double 

values = [20,30,60]

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
    operation :  dependentDecision name (const [0,20..60]) ;
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

-- 3 players with 1 auction slot, 2nd highest price, and 1 lottery slot with modified payment - truthful bidding - eq
-- generateIsEq $ equilibriumGame 2 1 1 lotteryPayment2 truthfulStrat

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




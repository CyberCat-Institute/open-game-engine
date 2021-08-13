{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.Auctions.DutchAuction where 


import Engine.Engine
import Preprocessor.Preprocessor
import Examples.Auctions.AuctionSupportFunctions



-- Create super-game including a tick-repeated version
-- 
{-
----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double

values = [20,30,60]

data Bid = Stop | Wait
  deriving (Eq,Show,Ord)

---------------------
-- 1 The actual games
-- 1.1 Initial games
-- Draws a value and creates a _value_ 
natureDrawsTypeStage = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : value ;
    returns   :  ;
    :-----:

    outputs   :  value ;
    returns   :    ;
  |]

-- Draws two values
nature2 = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : value1 ;
    returns   :  ;

    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : value2 ;
    returns   :  ;

    :-----:

    outputs   :  value1, value2 ;
    returns   :    ;
  |]

-- Creates an input parameter and puts into an open game
-- Purpose is for the initial context
startingTimer timer = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    : timer  ;
    feedback  :   ;
    operation : forwardFunction (\x -> x) ;
    outputs   : timer' ;
    returns   :  ;

    :-----:

    outputs   :  timer' ;
    returns   :    ;
  |]

-- Creates an input parameter and puts into an open game
-- Purpose is for the initial context
startingBid bid = [opengame|

    inputs    :   ;
    feedback  :   ;

    :-----:
    inputs    : bid  ;
    feedback  :   ;
    operation : forwardFunction (\x -> x) ;
    outputs   : bid' ;
    returns   :  ;

    :-----:

    outputs   : bid' ;
    returns   :    ;
  |]

-- 1.2. actual bidding parts
  
-- Have a base game, and iterate that game.
-- Individual bidding stage
biddingStage name = [opengame|

    inputs    :  value, ticker, decs  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  value, ticker, decs  ;
    feedback  :   ;
    operation :  dependentDecision name (const [Stop,Wait]) ;
    outputs   :  dec ;
    returns   :  payment  ;
    :---------------------------:

    outputs   :  dec ;
    returns   :  payment  ;
  |]


-- generate game with 2 players
biddingGame2  = [opengame|

    inputs    :  value1, value2, ticker, decs  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  value1, ticker, decs  ;
    feedback  :   ;
    operation :  biddingStage "Player1" ;
    outputs   :  dec1 ;
    returns   :  payoff1 value1 payments  ;

    inputs    :  value2, ticker, decs  ;
    feedback  :   ;
    operation :  biddingStage "Player2" ;
    outputs   :  dec2 ;
    returns   :  payoff2 value2 payments  ;

    inputs    :  ticker, decs, dec1, dec2  ;
    feedback  :   ;
    operation :  forwardFunction determinePayments  ;
    outputs   :  payments ;
    returns   :  ;

    inputs    :  ticker;
    feedback  :   ;
    operation :  forwardFunction transformTicker  ;
    outputs   :  newTicker ;
    returns   :  ;

    inputs    :  decs, dec1, dec2;
    feedback  :   ;
    operation :  forwardFunction transformDecisions  ;
    outputs   :  decsNew ;
    returns   :  ;

    :---------------------------:

    outputs   :  value1, value2, newTicker, decsNew ;
    returns   :   ;
  |]

------------------------
-- 2 Auxiliary functions
determinePayments (ticker,decs,dec1,dec2)
  | decs == Stop = (0,0) -- if the auction was stopped before, payoff is zero
  | ticker == 0  = (0,0) -- if the clock is over, payoff is zero for players
  | dec1 == Stop && dec2 == Stop = (ticker,0) -- TODO: make this stochastic; default now that the first player gets the good
  | dec1 == Stop = (ticker,0) -- first player stops the auction
  | dec2 == Stop = (0,ticker) -- second player stops the auction
  | otherwise    = (0,0)      -- none of the players stops the auction

payoff1 value (pay1,pay2)
  | pay1 > 0  = value - pay1
  | otherwise = 0

payoff2 value (pay1,pay2)
  | pay2 > 0  = value - pay2
  | otherwise = 0

transformTicker ticker = ticker - 1

transformDecisions (decs,dec1,dec2) =
  if elem Stop [decs,dec1,dec2]
     then Stop
     else Wait

-----------------------------
-- 3 Constructing the context


-- I need some starting context which determines the value types
-- Maybe I can create a context out of the play function?

testEvaluate context strat = evaluate biddingGame2 strat context
testPlay strat = play biddingGame2 strat


-- Given an optic and a state, extract the action.
extractForwardLensPart :: StochasticStatefulOptic
                             (Double, Double, Double, Bid)
                             ()
                             (Double, Double, Double, Bid)
                             ()
                       -> (Double, Double, Double, Bid)
                       -> Stochastic ((),(Double, Double, Double, Bid))
extractForwardLensPart (StochasticStatefulOptic
                          (v :: (Double, Double, Double, Bid)
                             -> Stochastic (z, (Double, Double, Double, Bid))) _) s = do
    (_,a) <- v s
    return ((),a)

-- Turn this into a context
-- Given an optic and a state, produce a context
opticToContext :: Show z => StochasticStatefulOptic
                             (Double, Double, Double, Bid)
                             ()
                             (Double, Double, Double, Bid)
                             ()
                       -> (Double, Double, Double, Bid)
                       -> StochasticStatefulContext
                             (Double, Double, Double, Bid)
                             ()
                             (Double, Double, Double, Bid)
                             ()
opticToContext optic s =
  let a = extractForwardLensPart optic s
      in StochasticStatefulContext a (\_ _-> pure ())

-- With an initial state, with a strategy, we can produce the next context.

testContext strat initialS = opticToContext (testPlay strat) initialS

-- Now let us plug this together with the play functionality
playToAction strat s = extractForwardLensPart (testPlay strat) s

-- Fix a context by hand first
contextFixed :: StochasticStatefulContext (Double, Double, Double, Bid) () (Double, Double, Double, Bid) ()
contextFixed = StochasticStatefulContext (pure ((),(30,30,60,Wait))) (\_ _-> pure ())

-- create context through play
-- But this requires mapping an optic to context
-- and then give manual numbers for the ticker start
-- focus on evaluating this first



playStartingTimer = play  (startingTimer 60) Nil

playStartingBid   = play (startingBid Wait) Nil


--aggregateOptic = testPlay >>>> playStartingTimer
-- StochasticStatefulContext
--          (Double, Double, Double, Bid) () (Double, Double, Double, Bid) ()
--           x () r ()

-- the initial optic gives () () r ()
-- StochasticStatefulContext :: (Show z) => Stochastic (z, s) -> (z -> a -> StateT Vector Stochastic b) -> StochasticStatefulContext s t a b



testLens x = lens (\y -> x) (\_ _ -> ())
--}

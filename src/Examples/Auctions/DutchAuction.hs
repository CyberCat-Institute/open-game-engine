{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Examples.Auctions.DutchAuction where

import qualified Control.Monad.Trans as ST (lift)
import Control.Monad.Trans.State.Strict
import OpenGames
import OpenGames.Preprocessor

-- Create super-game including a tick-repeated version
--

----------
-- A Model
----------

---------------
-- 0 Parameters

type Values = Double

type Started = Bool

-- fix overall clock - length of auction
clock = 60

hasTypeBeenDrawn :: Integer -> Bool
hasTypeBeenDrawn ticker =
  if ticker < clock then True else False

ignoreUpdateTypes' :: (Started, Double, Double, Double, Double) -> (Double, Double)
ignoreUpdateTypes' (True, old1, old2, _, _) = (old1, old2)
ignoreUpdateTypes' (False, _, _, new1, new2) = (new1, new2)

ignoreUpdateTypes :: (Started, Double, Double, Double, Double) -> (Double, Double)
ignoreUpdateTypes _ = (1, 1)

values = [10, 20, 30]

data Bid = Stop | Wait
  deriving (Eq, Show, Ord)

---------------------
-- 1 The actual games
-- 1.1 Initial games
-- Draws a value and creates a _value_
natureDrawsTypeStage =
  [opengame|

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

-- Given ticker, determine if game is already running. If so, feedforward the old values. If no, draw values and forward them.
nature2 ::
  OpenGame
    StochasticStatefulOptic
    StochasticStatefulContext
    ('[])
    ('[])
    (Integer, Double, Double)
    ()
    (Double, Double)
    ()
nature2 =
  [opengame|

    inputs    : ticker, valueOld1, valueOld2 ;
    feedback  :   ;

    :-----:
    inputs    :  ticker ;
    feedback  :   ;
    operation : forwardFunction hasTypeBeenDrawn ;
    outputs   : started ;
    returns   :  ;

    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : valueNew1 ;
    returns   :  ;

    inputs    :   ;
    feedback  :   ;
    operation : nature (uniformDist values) ;
    outputs   : valueNew2 ;
    returns   :  ;

    inputs    :  started, valueOld1, valueOld2, valueNew1, valueNew2 ;
    feedback  :   ;
    operation : forwardFunction ignoreUpdateTypes' ;
    outputs   : valuesNew ;
    returns   :  ;
    :-----:

    outputs   : valuesNew ;
    returns   :    ;
  |]

-- 1.2. actual bidding parts
-- Have a base game, and iterate that game.
-- Individual bidding stage
biddingStage ::
  String ->
  OpenGame
    StochasticStatefulOptic
    StochasticStatefulContext
    '[Kleisli Stochastic (Double, Integer, Bid) Bid]
    '[[DiagnosticInfoBayesian (Double, Integer, Bid) Bid]]
    (Double, Integer, Bid)
    ()
    Bid
    Double
biddingStage name =
  [opengame|

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
biddingGameComplete =
  [opengame|

    inputs    :  value1Old, value2Old, ticker, decs  ;
    feedback  :   ;

    :---------------------------:
    inputs    :  ticker, value1Old, value2Old   ;
    feedback  :   ;
    operation :  nature2 ;
    outputs   :  (value1, value2) ;
    returns   :  ;


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

    outputs   :  value1, value2, (newTicker - 1), decsNew ;
    returns   :   ;
  |]

------------------------
-- 2 Auxiliary functions
determinePayments :: (Integer, Bid, Bid, Bid) -> (Double, Double)
determinePayments (ticker, decs, dec1, dec2)
  | decs == Stop = (0, 0) -- if the auction was stopped before, payoff is zero
  | ticker == 0 = (0, 0) -- if the clock is over, payoff is zero for players
  | dec1 == Stop && dec2 == Stop = (fromInteger ticker, 0) -- FIXME: make this stochastic; default now that the first player gets the good
  | dec1 == Stop = (fromInteger ticker, 0) -- first player stops the auction
  | dec2 == Stop = (0, fromInteger ticker) -- second player stops the auction
  | otherwise = (0, 0) -- none of the players stops the auction

payoff1 value (pay1, pay2)
  | pay1 > 0 = value - pay1
  | otherwise = 0

payoff2 value (pay1, pay2)
  | pay2 > 0 = value - pay2
  | otherwise = 0

transformTicker ticker = ticker - 1

transformDecisions (decs, dec1, dec2) =
  if elem Stop [decs, dec1, dec2]
    then Stop
    else Wait

-----------------------
-- Continuation payoffs

-- Extract continuation
extractContinuation :: StochasticStatefulOptic (Double, Double, Integer, Bid) () (Double, Double, Integer, Bid) () -> (Double, Double, Integer, Bid) -> StateT Vector Stochastic ()
extractContinuation (StochasticStatefulOptic v u) x = do
  (z, a) <- ST.lift (v x)
  u z ()

-- Extract next state (action)
extractNextState :: StochasticStatefulOptic (Double, Double, Integer, Bid) () (Double, Double, Integer, Bid) () -> (Double, Double, Integer, Bid) -> Stochastic (Double, Double, Integer, Bid)
extractNextState (StochasticStatefulOptic v _) x = do
  (z, a) <- v x
  pure a

-- Determine continuation payoff with the same repeated strategy
determineContinuationPayoffs 1 strat action = pure ()
determineContinuationPayoffs iterator strat action = do
  extractContinuation executeStrat action
  nextInput <- ST.lift $ extractNextState executeStrat action
  determineContinuationPayoffs (pred iterator) strat nextInput
  where
    executeStrat = play biddingGameComplete strat

----------
-- Context

-- Context used for the evaluation of the pathological end state
contextCont iterator strat initialAction = StochasticStatefulContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffs iterator strat action)

-------------
-- Strategies

-- Add strategy for stage game
stageStrategy :: Kleisli Stochastic (Double, Integer, Bid) Bid
stageStrategy =
  Kleisli $
    ( \case
        (_, _, Stop) -> playDeterministically Stop
        (v, ticker, _) -> if v < fromInteger ticker then playDeterministically Wait else playDeterministically Stop
    )

-- Aggregate the strategies into a profile
strategyTuple = stageStrategy :- stageStrategy :- Nil

--------------
-- Equilibrium

-- Pathological end state
repeatedCompleteGameEq iterator strat initialAction = evaluate biddingGameComplete strat context
  where
    context = contextCont iterator strat initialAction

-- Show output pathological end game
eqOutput iterator strat initialAction = generateIsEq $ repeatedCompleteGameEq iterator strat initialAction

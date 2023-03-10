{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module OpenGames.Contracts.Player where

import OpenGames.Contracts.Amm
import OpenGames.Engine.OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.Engine

import Numeric.Probability.Distribution (uniform)

swapStrategy :: Double -> [Double]
swapStrategy n = [0, 1 .. n]

bigPayoff finalUSD initialUSD swappedUSD
  = finalUSD + initialUSD - swappedUSD

actionSpaceTXs :: Bounds -> [Transaction]
actionSpaceTXs = undefined

$(act2OG "amm.act")

actGame = amm (AmmConfig { intRange = [0 .. 100]
                         , feeRange = [0..10]})

chooseTransactionAndFee name upperBound bounds =
  [opengame|
  inputs: state ;
  feedback: ;

  :------:

  inputs :  state ;
  operation : dependentDecision name $ const $ actionSpaceTXs bounds;
  outputs : tx;
  returns : 0 ;

  inputs : state, tx ;
  operation : dependentDecision name $ const $ actionSpaceFee upperBound ;
  outputs : fee;
  returns : 0 ;

  inputs : tx,fee ;
  operation : forwardFunction $ uncurry combineTXAndFee ;
  outputs : txWithFee ;

  :------:
  outputs : txWithFee ;
  returns : ;

|]

-- generate n amm (with n tokens) | OK
--
-- generate m transactions with those n amms | <---
--
-- have a coordinator picks 1 arrangement of m | OK
--
-- Transactions = Transaction Address [ARgs]
--
-- [ T "Loan" [100,200]
-- , T "Token1Swap1" [100]
-- , T "Token1Swap2" [3000]
-- ]

ammPlayer initialUSD = [opengame|
  inputs    : r1, r2, r1', r2' ;
  feedback  : ;
  :---------:
  inputs    : r1, r2 ;
  operation : dependentDecision "Marx" (const (swapStrategy initialUSD)) ;
  outputs   : swappableUSD ;
  returns   : bigPayoff (proj finalUSD) initialUSD swappableUSD ;

  inputs    : (r1, r2), Swap0 swappableUSD;
  feedback  : ;
  operation : amm ;
  outputs   : eur, st' ;
  returns   : ;

  inputs    : transactions ;
  feedback  : ;
  operation : and (amm "token1") (amm "token2");
  outputs   : finalState;
  returns   : ;

  :---------:
  outputs   : ;
  returns   : ;
|]

-- questions:
-- - What do we improve in this model next?
--   - sandwich? (different example)
--   - another "from act" example
--   - betting contract from act?
--     - betting on the exchange rate of an AMM
--     - clockwork finance example
--     - move on to token swap
-- - What do we automate from Act ?
--   - extract name state fields
--   - what about rollback?
--   - strategy stealing?
--   - failing transactions added to the global state?
--   - generate players ?
--   - gas fees + mem pool + Bribable coordinator
--
--   To do in general:
--   - Work on the common infrastructure around modelling situations
--     - coordinator
--     - calling subcontract
--     - bribes
--
--   Next week:
--   - Work on another act example, maybe draw from clockwork finance
--     - Create an act program for a full AMM with setup
--     - Create an act program for betting
--
--  ## 10.03
--  - We have a game with multiple AMM and a way to dispatch transactions
--  todo:
--  - send multiple transactions and check they are executed correctly
--  - game to find which transaction order would optimise the payoff
--      - run this for 1 amm
--  - Work toward having common state between AMMs
--  - same operations but now find how to

ctx = StochasticStatefulContext @() (pure ((), (800,1000,1000,800))) (\_ _ -> return ())

ev = evaluate (ammPlayer 10) ((pureAction 1) :- Nil) ctx

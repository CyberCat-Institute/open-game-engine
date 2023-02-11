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

  inputs    : (r1', r2'), Swap1 (proj eur) ;
  feedback  : ;
  operation : amm ;
  outputs   : finalUSD, st'';
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
--   next week:
--   - work on another act example, maybe draw from clockwork finance

ctx = StochasticStatefulContext @() (pure ((), (800,1000,1000,800))) (\_ _ -> return ())

ev = evaluate (ammPlayer 10) ((pureAction 1) :- Nil) ctx

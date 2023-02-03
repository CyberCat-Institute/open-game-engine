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


ctx = StochasticStatefulContext @() (pure ((), (800,1000,1000,800))) (\_ _ -> return ())

ev = evaluate (ammPlayer 10) (pure 1 :- Nil) ctx

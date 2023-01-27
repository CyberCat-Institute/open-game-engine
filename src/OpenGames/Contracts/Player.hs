{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module OpenGames.Contracts.Players where

import OpenGames.Contracts.Amm
import OpenGames.Engine.OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.Engine

-- amm1 = (initAmm 10 5 &&& id) >>> amm
--
-- amm2 = initAmm 20 20 >>> amm

swapStrategy :: Double -> [Double]
swapStrategy n = [0, 1 .. n]

bigPayoff usd1 usd2 inv = usd1 + usd2 - inv

player usd = [opengame|
  inputs   : r1, r2, r1', r2', usd ;
  feedback : ;
  :-------:

  operation : dependentDecision "Marx" (const (swapStrategy usd)) ;
  outputs   : d ;
  returns   : bigPayoff realDollars usd d ;

  inputs  : (r1, r2), Swap0 d;
  feedback : ;
  operation : amm ;
  outputs : st', eur ;
  returns : ;

  inputs    : (r1', r2'), Swap1 (proj eur) ;
  feedback  : ;
  operation : amm ;
  outputs : realDollars ;
  returns : ;

  :-------:
  outputs: ;
  returns : ;
|]


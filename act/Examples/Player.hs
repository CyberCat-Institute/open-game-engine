{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Player where

import Act.Execution
import Act.Prelude
import Data.List
import EVM.ABI (AbiType (..))
import Examples.AmmGenerated
import OpenGames.Engine.Engine
import OpenGames.Engine.OpenGames
import OpenGames.Preprocessor

-- This combines two contracts with non-shared state
twoAmms = combine (unionContracts ("amm1", ammContract) ("amm2", ammContract))

swapStrategy :: Int -> [Int]
swapStrategy n = [0, 1 .. n]

bigPayoff finalUSD initialUSD swappedUSD =
  finalUSD + initialUSD - swappedUSD

swap0 :: Int -> Transaction
swap0 d = Transaction "" "swap0" [AbiUIntType d]

swap1 :: Int -> Transaction
swap1 d = Transaction "" "swap1" [AbiUIntType d]

diffEur :: AmmState -> AmmState -> Int
diffEur (AmmState old _) (AmmState new _) = new - old

diffUsd :: AmmState -> AmmState -> Int
diffUsd (AmmState old _) (AmmState new _) = new - old

playerAutomatic usd =
  [opengame|
  inputs   : ammSt1, ammSt2 ;
  feedback : ;
  :-------:

  operation : dependentDecision "Marx" (const (swapStrategy usd)) ;
  outputs   : d ;
  returns   : fromIntegral ((diffUsd ammSt2 st''));

  inputs    : ammSt1, swap0 d;
  feedback  : ;
  operation : forwardFunction (uncurry ammContract) ;
  outputs   : st' ;
  returns   : ;

  inputs    : ammSt2, swap1 (diffEur ammSt1 st') ;
  feedback  : ;
  operation : forwardFunction (uncurry ammContract) ;
  outputs   : st'' ;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

contracts = (combine [("amm1", ammContract)])

allTransaction =
  [ Transaction "amm1" "swap0" [AbiUIntType 5],
    Transaction "amm1" "swap1" [AbiUIntType 10],
    Transaction "amm1" "swap0" [AbiUIntType 15]
  ]

transactionOrders :: [[Transaction]]
transactionOrders = permutations allTransaction

twoTokensPayoff :: AmmState -> AmmState -> Int
twoTokensPayoff (AmmState t1old t2old) (AmmState t1new t2new) =
  (t1new - t1old) + (t2new - t2old)

swapSequence =
  [opengame|
  inputs   : ammSt1 ;
  feedback : ;

  :-------:

  operation : dependentDecision "Marx" (const transactionOrders) ;
  outputs   : transactions ;
  returns   : fromIntegral (twoTokensPayoff ammSt1 finalState) ;

  inputs    : transactions, ammSt1;
  feedback  : ;
  operation : forwardFunction (uncurry contracts ) ;
  outputs   : finalState;
  returns   : ;

  :-------:
  outputs: ;
  returns : ;
|]

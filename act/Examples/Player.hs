{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Player where

import Act
import Data.List
import Examples.AmmGenerated
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

-- This combines two contracts with non-shared state
twoAmms = combine (unionContracts ("amm1", ammContract) ("amm2", ammContract))

swapStrategy :: Word256 -> [Word256]
swapStrategy n = [0, 1 .. n]

bigPayoff finalUSD initialUSD swappedUSD =
  finalUSD + initialUSD - swappedUSD

swap0 :: Word256 -> Transaction
swap0 d = Transaction "" "swap0" [AbiUInt 64 d]

swap1 :: Word256 -> Transaction
swap1 d = Transaction "" "swap1" [AbiUInt 64 d]

diffEur :: AmmState -> AmmState -> Word256
diffEur (AmmState old _) (AmmState new _) = new - old

diffUsd :: AmmState -> AmmState -> Word256
diffUsd (AmmState old _) (AmmState new _) = new - old

playerAutomatic usd =
  [opengame|
  inputs   : ammSt1, ammSt2 ;
  feedback : ;
  :-------:

  operation : dependentDecision "Marx" (const (swapStrategy usd)) ;
  outputs   : d ;
  returns   : fromIntegral (diffUsd ammSt2 st'');

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

contracts = (combine [("amm2", ammContract), ("amm1", ammContract)])

allTransactionSwap =
  [ Transaction "amm1" "swap1" [AbiUInt 64 5],
    Transaction "amm2" "swap0" [AbiUInt 64 10]
  ]

allTransaction x =
  [ Transaction "amm2" "swap0" [AbiUInt 64 x] | x <- [1 .. x]
  ]

transactionOrders :: [[Transaction]]
transactionOrders = permutations allTransactionSwap

twoTokensPayoff :: AmmState -> AmmState -> Word256
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

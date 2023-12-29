{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Player where

import Act
import Data.List
import OpenGames.Engine.Engine
import OpenGames.Preprocessor

$(act2OG "act-programs/amm.act")

-- This combines two contracts with non-shared state
twoAmms = combine (unionContracts ("amm1", ammContract) ("amm2", ammContract))

swapStrategy :: Word256 -> [Word256]
swapStrategy n = [0, 1 .. n]

bigPayoff finalUSD initialUSD swappedUSD =
  finalUSD + initialUSD - swappedUSD

swap0 :: Word256 -> Transaction
swap0 d = Transaction "amm1" "swap0" [AbiUInt 64 d]

swap1 :: Word256 -> Transaction
swap1 d = Transaction "amm2" "swap1" [AbiUInt 64 d]

bundles :: Word256 -> [[Transaction]]
bundles swapLimit = [[swap0 n, swap1 n] | n <- [0, 1 .. swapLimit]]

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
  outputs   : ;
  returns   : ;
|]

initSendAndRun x = twoAmms x (AmmState 50 50, AmmState 50 50)

balance :: (AmmState, AmmState) -> String -> Double
balance (st1, st2) _ = fromIntegral (reserve0 st1 + reserve0 st2)

actDecision name strategies =
  [opengame| inputs : observedInput ;
  :---:

  inputs : observedInput ;
  operation : dependentDecision name (const strategies) ;
  outputs   : tx ;
  returns  : balance finalState name ;

  :---:
  outputs : tx ;
  returns : finalState;
|]

append = (++)

runBlockchain =
  [opengame|
  inputs : ;
  :---:

  operation : actDecision "Marx" (bundles 10) ;
  outputs : allTx ;
  returns : finalState ;

  inputs : allTx ;
  operation : fromFunctions (initSendAndRun) id ;
  outputs : finalState ;
|]

foo = evaluate runBlockchain (Kleisli (const (pure [swap0 0, swap1 0])) :- Nil) void

ctx =
  StochasticStatefulContext @()
    (pure ((), (AmmState 8 10, AmmState 10 8)))
    (\_ _ -> return ())

ev = evaluate (playerAutomatic 10) ((pureAction 1) :- Nil) ctx

ctx1 =
  StochasticStatefulContext @()
    (pure ((), (AmmState 10 10)))
    (\_ _ -> return ())

ev1 = evaluate swapSequence ((pureAction (reverse (allTransactionSwap))) :- Nil) ctx1

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Contracts.Amm where

import OpenGames
import OpenGames.Preprocessor

data Transaction = Swap0 Double | Swap1 Double
  deriving (Show)
data Result = Swap0Out () | Swap1Out ()
  deriving (Show)

type ContractState = (Double, Double)

inRange :: Double -> Double -> Bool
inRange _ _ = True

swapWithAmount :: Transaction -> ContractState -> (Result, ContractState)
swapWithAmount (Swap0 amt) st@(reserve0, reserve1) =
    if inRange amt reserve0
      then
          (Swap0Out (), (reserve0 + amt
          ,(reserve0 * reserve1) / (reserve0 + amt) + 1))
      else (Swap0Out (), st)
swapWithAmount (Swap1 amt) st@(reserve0, reserve1) =
    if inRange amt reserve0
      then
    (Swap1Out (), ((reserve0 * reserve1) / (reserve1 + amt) + 1
    ,reserve1 + amt))
      else (Swap1Out (), st)

amm = [opengame|
  inputs: transaction, state ;
  feedback: ;

  :------:

  inputs : transaction, state ;
  operation : forwardFunction $ uncurry swapWithAmount ;
  outputs : output, state' ;

  :------:
  outputs : output, state';
  returns : ;
|]


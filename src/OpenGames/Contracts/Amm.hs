{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Contracts.Amm where

import OpenGames
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGames
import OpenGames.Preprocessor

-- transaction { gas :: Int, Function :: String, Arg :: Int }
data Transaction = Swap0 Double | Swap1 Double
  deriving (Show)

data Result = Swap0Out {g :: Double} | Swap1Out {g' :: Double}
  deriving (Show)

data Strategy = Strategy Transaction
  deriving (Show)


proj :: Result -> Double
proj (Swap1Out n) = n
proj (Swap0Out n) = n

-- act questions :
--   - add gas?
--   - translate functions from act to OG and add gas to them

type ContractState = (Double, Double)

inRange :: Double -> Double -> Bool
inRange _ _ = True

swapWithAmount :: ContractState -> Transaction  -> (Result, ContractState)
swapWithAmount st@(reserve0, reserve1) (Swap0 amt)  =
    if inRange amt reserve0
      then
        ( Swap1Out (reserve1 - ((reserve0 * reserve1) / (reserve0 + amt) + 1))
        , ( reserve0 + amt
          , (reserve0 * reserve1) / (reserve0 + amt) + 1
          )
        )
      else (Swap1Out 0, st)
swapWithAmount st@(reserve0, reserve1) (Swap1 amt)  =
    if inRange amt reserve1
      then
        ( Swap0Out (reserve0 - ((reserve0 * reserve1) / (reserve1 + amt) + 1))
        , ( (reserve0 * reserve1) / (reserve1 + amt) + 1
          , reserve1 + amt
          )
        )
      else (Swap0Out 0, st)

initAmm :: Double
     -> Double
     -> OpenGames.Engine.OpenGames.OpenGame
          OpenGames.Engine.OpticClass.StochasticStatefulOptic
          OpenGames.Engine.OpticClass.StochasticStatefulContext
          '[]
          '[]
          ()
          ()
          (Double, Double)
          ()

initAmm r1 r2 = forwardFunction (const (r1, r2))

amm = [opengame|
  inputs: state, transaction ;
  feedback: ;

  :------:

  inputs : state, transaction ;
  operation : forwardFunction $ uncurry swapWithAmount ;
  outputs : state', output  ;

  :------:
  outputs : state', output ;
  returns : ;
|]


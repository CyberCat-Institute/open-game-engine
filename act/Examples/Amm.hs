{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.Amm where

import Act.Execution
import Act.Prelude

import OpenGames
import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGames
import OpenGames.Preprocessor

arity :: Address -> EVMType
arity = undefined

type Address = String
type EVMType = String
--
-- transaction { gas :: Int, Function :: String, Arg :: Int }
-- data Transaction = Transaction Address [EVMType]
--   deriving (Show)

data Result = Swap0Out {g :: Double} | Swap1Out {g' :: Double} -- derived from act API
  deriving (Show)

data Strategy = Strategy Transaction
  deriving (Show)


proj :: Result -> Double
proj (Swap1Out n) = n
proj (Swap0Out n) = n

-- act questions :
--   - add gas?
--   - translate functions from act to OG and add gas to them

-- newtype GlobalState = GlobalState { getState :: Map String Double }

newtype Token = Token { getToken :: (String, Double) }
  deriving Show via (String, Double)

type ContractState = (Token, Token) -- derived from act Store
type MemPool = [Transaction] -- ????
-- type GlobalState = (Mempool, ContractState) -- ??????????????????

inRange :: Double -> Double -> Bool
inRange _ _ = True

ammFunction :: ContractState -> Transaction  -> ContractState
ammFunction = undefined
-- swapWithAmount st@(reserve0, reserve1) (Swap0 amt)  =
--     if inRange amt reserve0 -- derived from preconditions
--       then
--         -- derived from implementation
--         ( Swap1Out (reserve1 - ((reserve0 * reserve1) / (reserve0 + amt) + 1))
--         , ( reserve0 + amt
--           , (reserve0 * reserve1) / (reserve0 + amt) + 1
--           )
--         )
--       else (Swap1Out 0, st)
-- swapWithAmount st@(reserve0, reserve1) (Swap1 amt)  =
--     if inRange amt reserve1
--       then
--         ( Swap0Out (reserve0 - ((reserve0 * reserve1) / (reserve1 + amt) + 1))
--         , ( (reserve0 * reserve1) / (reserve1 + amt) + 1
--           , reserve1 + amt
--           )
--         )
--       else (Swap0Out 0, st)

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

combined :: [Transaction] -> (ContractState, ContractState) -> (ContractState, ContractState)
combined = combine (unionContracts ("amm1", ammFunction) ("amm2", ammFunction))

amm = [opengame|
  inputs: state, transaction ;
  feedback: ;

  :------:

  inputs : state, transaction ;
  operation : forwardFunction $ uncurry combined ;
  outputs : output, state' ;

  :------:
  outputs : output, state' ;
  returns : ;
|]

-- questions for david:
-- - how does act deal with BC state? for example blockchain number

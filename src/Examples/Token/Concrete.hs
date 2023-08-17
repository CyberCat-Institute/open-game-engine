{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.Token.Concrete where

-- - concrete token
-- - abstract token
-- -

import Data.HashMap as M
import Data.Maybe
import OpenGames
import OpenGames.Preprocessor

type Address = String

type TokenState = (Map Address Int)

balance :: Address -> TokenState -> Int
balance add st = fromMaybe 0 (M.lookup add (st))

set :: Address -> Int -> TokenState -> TokenState
set = M.insert

transfer :: Address -> Address -> Int -> TokenState -> TokenState
transfer source target amount st =
  let local = balance source st
   in if local < amount
        then st
        else
          let s1 = set source (local - amount) st
           in set target (balance target s1 + amount) s1

data Action
  = Transfer Address Address Int
  deriving (Show, Eq, Ord)

performOperation :: (Action, TokenState) -> TokenState
performOperation (Transfer from to x, st) = transfer from to x st

generateActions :: [Address] -> [Int] -> [Action]
generateActions actors amounts = do
  p1 <- actors
  p2 <- actors
  a <- amounts
  pure (Transfer p1 p2 a)

at :: [a] -> Int -> a
at = (!!)

initialState :: TokenState
initialState = M.insert "player2" 20 empty

defaultContext :: StochasticStatefulContext TokenState () TokenState ()
defaultContext =
  let h = return ((), initialState)
      k () = const $ return ()
   in StochasticStatefulContext h k

transferGame playerIndex addresses amounts =
  [opengame|

  inputs : state ;

  :---:

  inputs : state ;
  operation : dependentDecision (at addresses playerIndex) (const $ generateActions addresses amounts) ;
  outputs : action ;
  returns : fromIntegral $ balance (at addresses playerIndex) newState ;

  inputs : action, state ;
  operation : fromFunctions performOperation id ;
  outputs : newState ;

  :---:

  outputs : newState; |]

defaultAmounts :: [Int]
defaultAmounts = [0, 5, 10]

defaultAddresses = ["player1", "player2", "player3"]

player0 = transferGame 0 defaultAddresses defaultAmounts

player1 = transferGame 1 defaultAddresses defaultAmounts

player2 = transferGame 2 defaultAddresses defaultAmounts

evaluated =
  evaluate
    (player0 >>> player1 >>> player2)
    ( pure (Transfer "player2" "player1" 10)
        :- pure (Transfer "player1" "player2" 10)
        :- pure (Transfer "player2" "player3" 10)
        :- Nil
    )
    defaultContext

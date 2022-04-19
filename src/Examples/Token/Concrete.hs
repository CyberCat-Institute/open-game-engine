{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
module Examples.Token.Concrete where

-- - concrete token
-- - abstract token
-- -

import OpenGames
import OpenGames.Preprocessor
import Control.Monad.Identity
import OpenGames.Engine.Nat
import Data.HashMap as M
import Data.Maybe
import Data.List
import GHC.Utils.Misc

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
        else let s1 = set source (local - amount) st
             in set target (balance target s1 + amount) s1


data Action
  = Transfer Address Address Int
  deriving Show

decisionSymbolic :: [Action] -> OpenGame StochasticStatefulOptic StochasticStatefulContext '[TokenState -> Action] '[] TokenState () Action Int
decisionSymbolic = undefined

defaultAmounts :: [Int]
defaultAmounts = [0, 5, 10]

performOperation :: (Action, TokenState) -> TokenState
performOperation (Transfer from to x, st) = transfer from to x st

generateActions :: [Address] -> [Int] -> [Action]
generateActions actors amounts = fmap (uncurry3 Transfer) (concat $ permutations (zip3 actors actors amounts))

at :: [a] -> Int -> a
at = (!!)

transferGame playerIndex addresses amounts = [opengame|

  inputs : state ;

  :---:

  inputs : state ;
  operation : decisionSymbolic (generateActions addresses amounts ) ;
  outputs : action ;
  returns : balance (at addresses playerIndex) state ;

  inputs : action, state ;
  operation : fromFunctions performOperation id ;
  outputs : newState ;

  :---:

  outputs : newState; |]



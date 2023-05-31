module Act.Execution where

import Act.Prelude
import Data.Bifunctor

-- An ACT contract is a pair of a _name_ and a function that updates
-- a state `s` given a transaction
type ActContract s = (String, s -> Transaction -> s)

unionContracts ::
  ActContract s1 ->
  ActContract s2 ->
  [ActContract (s1, s2)]
unionContracts (n1, f1) (n2, f2) =
  [(n1, focus fst (\(x, y) x' -> (x', y)) f1), (n2, focus snd (\(x, y) y' -> (x, y')) f2)]
  where
    focus :: (t -> s) -> (t -> s -> t) -> (s -> Transaction -> s) -> (t -> Transaction -> t)
    focus view update fn st trans = update st (fn (view st) trans)

-- Given an arbitrary number of contracts we can generate a state-updating function that
-- performs a list of transaction on the given set of contracts.
combine :: [ActContract s] -> [Transaction] -> s -> s
combine contracts [transaction] globalState =
  let Just trans = Prelude.lookup (contract transaction) contracts
   in trans globalState transaction
combine contracts (t : ts) globalState =
  case Prelude.lookup (contract t) contracts of
    Just trans -> let newState = trans globalState t in combine contracts ts newState
    Nothing -> error ("got illegal transaction " ++ show t)

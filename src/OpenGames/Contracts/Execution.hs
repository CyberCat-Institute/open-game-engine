module OpenGames.Contracts.Execution where

import Data.Bifunctor

data Transaction = T { name :: String , args :: [Int] }
  deriving Show

type ActContract s = (String, (s, Transaction) -> s)

-- $(instanciateGame ("amm.act", "Token1", addr1, addr2) ("amm.act", "Token2", addr2, addr3))

unionContracts :: ActContract s1
               -> ActContract s2
               -> [ActContract (s1, s2)]
unionContracts (n1, f1) (n2, f2) =
  [(n1, focus fst (\(x, y) x' -> (x', y)) f1), (n2, focus snd (\(x, y) y' -> (x, y')) f2)]
  where
    focus :: (t -> s) -> (t -> s -> t) -> ((s, Transaction) -> s) -> ((t, Transaction) -> t)
    focus view update fn st = update (fst st) (fn (first view st))


combine :: [ActContract s] -> [Transaction] -> s -> s
combine contracts [transaction] globalState =
  let Just trans = Prelude.lookup (name transaction) contracts
  in (trans (globalState, transaction))
combine contracts (t : ts) globalState =
  let Just trans = Prelude.lookup (name t) contracts
      newState = trans (globalState, t)
  in (combine contracts ts newState)

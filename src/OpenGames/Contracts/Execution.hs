

data Transaction = T { name :: String , args :: [Int] }

type ActContract s r = (String, (s, Transaction) -> (r, s))

combine :: [ActContract s r] -> [Transaction] -> s -> ([r], s)
combine contracts [transaction] globalState =
  let Just trans = Prelude.lookup (name transaction) contracts
  in first pure (trans (globalState, transaction))
combine contracts (t : ts) globalState =
  let Just trans = Prelude.lookup (name t) contracts
      (result, newState) = trans (globalState, t)
  in first (result :) (combine contracts ts newState)

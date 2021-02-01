{-# LANGUAGE TypeOperators, DataKinds, GADTs, KindSignatures, TypeFamilies #-}

-- Everything in this file was written by Sjoerd Visscher

module OpenGames.Engine.TLL where

import Prelude hiding ((++))

infixr 6 :-
data List ts where
  Nil :: List '[]
  (:-) :: t -> List ts -> List (t ': ts)

type family (++) (as :: [*]) (bs :: [*]) :: [*]  where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

class Unappend as where
  unappend :: List (as ++ bs) -> (List as, List bs)

instance Unappend '[] where
  unappend bs = (Nil, bs)

instance Unappend as => Unappend (a ': as) where
  unappend (a :- abs) = case unappend abs of (as, bs) -> (a :- as, bs)

(++) :: List as -> List bs -> List (as ++ bs)
(++) Nil bs = bs
(++) (a :- as) bs = a :- as ++ bs

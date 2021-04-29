{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                 #-}


-- Parts of this file were written by Sjoerd Visscher 

module Engine.TLL
  ( List(..)
  , Apply(..)
  , Unappend(..)
  , MapL(..)
  , FoldrL(..)
  , ConstMap(..)
  , SequenceList(..)
  , Natural(..)
  , IndexList(..)
  , type (+:+)
  , (+:+)
  ) where

import Control.Applicative
import Data.Kind

infixr 6 ::-
data List ts where
  Nil :: List '[]
  (::-) :: t -> List ts -> List (t ': ts)

instance Show (List '[]) where
    show Nil = "Nil"

instance (Show (List as), Show a)
    => Show (List (a ': as)) where
    show (a ::- rest) =
        show a ++ " ::- " ++ show rest


type family (+:+) (as :: [*]) (bs :: [*]) :: [*] where
  '[] +:+ bs = bs
  (a ': as) +:+ bs = a ': (as +:+ bs)

(+:+) :: List as -> List bs -> List (as +:+ bs)
(+:+) Nil bs = bs
(+:+) (a ::- as) bs = a ::- as +:+ bs


class Unappend as where
  unappend :: List (as +:+ bs) -> (List as, List bs)

instance Unappend '[] where
  unappend bs = (Nil, bs)

instance Unappend as => Unappend (a ': as) where
  unappend (a ::- abs) = case unappend abs of (as, bs) -> (a ::- as, bs)

---------------------------------
-- Operations to transform output
-- Preliminary apply class

class Apply f a b where
  apply :: f -> a -> b


-- Map
class MapL f xs ys where
  mapL :: f -> List xs -> List ys

instance MapL f '[] '[] where
  mapL _ _ = Nil


instance (Apply f x y, MapL f xs ys)
  => MapL f (x ': xs) (y ': ys) where
  mapL f (x ::- xs) = apply f x ::- mapL f xs

-- Foldr
class FoldrL f acc xs where
  foldrL :: f -> acc -> List xs -> acc

instance FoldrL f acc '[] where
  foldrL _ acc _ = acc

instance (Apply f x (acc -> acc), FoldrL f acc xs)
  => FoldrL f acc (x ': xs) where
  foldrL f acc (x ::- xs) = apply f x $ foldrL f acc xs

type family ConstMap (t :: *) (xs :: [*]) :: [*] where
  ConstMap _      '[]  = '[]
  ConstMap t (x ': xs) = t ': (ConstMap t xs)


----------------------------------------
-- Features to ease feeding back outputs
--
class Applicative m => SequenceList m a b | a -> b, m b -> a where
    sequenceListA :: List a -> m (List b)

instance Applicative m => SequenceList m '[] '[] where
    sequenceListA _ = pure Nil

instance (Applicative m, SequenceList m as bs) => SequenceList m (m a ': as) (a ': bs) where
    sequenceListA (a ::- b) = liftA2 (::-) a (sequenceListA b)


-- Indexing on the list 

data Nat = Z | S Nat

data Natural a where
  Zero :: Natural 'Z
  Succ :: Natural a -> Natural ('S a)


class IndexList (n :: Nat) (xs :: [Type]) (i :: Type) | n xs -> i where
   fromIndex :: Natural n -> List xs -> i

instance IndexList Z (x ': xs) x where
   fromIndex Zero (x ::- _) = x

instance IndexList n xs a => IndexList (S n) (x ': xs) a where
   fromIndex (Succ n) (_ ::- xs) = fromIndex n xs



{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Engine.Interleaving where

import Data.Either.Extra (fromEither)

-- import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.TLL
import OpenGames.Engine.OpenGames
import OpenGames.Engine.OpticClass
import OpenGames.Engine.Nat

(+++) :: OpenGame o c m a b x s y r
      -> OpenGame o c n a' b' x' s y' r
      -> OpenGame o c (Add m n) (Flatten a a') (Flatten b b') (Either x x') s (Either y y') r
(+++) = undefined

fromBool :: a -> Bool -> Either a a
fromBool x True = Right x
fromBool x False = Left x

interleave2 :: forall o c n m a a' b b' x x' y y' s s' r r'.
              (Optic o, Context c o, Show y, Show x, Show y', Show x')
           => OpenGame o c n a b (x, Maybe y') s y r
           -> OpenGame o c m a' b' (x', Maybe y) s' y' r'
           -> OpenGame o c (Add n m)(Flatten a a') (Flatten (Maybe b) (Maybe b')) ((x, x'), Bool) (s, s') (y, y') (r, r')
interleave2 g h = reindex (\aa' -> case unflatten aa' of (a :: a, a' :: a') -> flatten (flatten (a, a'), flatten (a', a))) (const _) undefined (fromFunctions (uncurry fromBool) id >>> (thing1 +++ thing2) >>> fromFunctions fromEither id) where
   thing1 = [opengame|
      inputs: x, x' ;
      feedback: s, s' ;

      :---:

      inputs: x, Nothing ;
      feedback : s ;
      operation: g ;
      outputs : y ;
      returns : r ;

      inputs : x', Just y ;
      feedback : s' ;
      operation : h ;
      outputs : y' ;
      returns : r' ;

      :---:

      outputs : y, y' ;
      returns : r, r' ;
   |]
   thing2 = [opengame|
      inputs: x, x' ;
      feedback: s, s' ;

      :---:

      inputs: x', Nothing ;
      feedback : s' ;
      operation: h ;
      outputs : y' ;
      returns : r' ;

      inputs : x, Just y' ;
      feedback : s ;
      operation : g ;
      outputs : y ;
      returns : r ;

      :---:

      outputs : y, y' ;
      returns : r, r' ;
   |]

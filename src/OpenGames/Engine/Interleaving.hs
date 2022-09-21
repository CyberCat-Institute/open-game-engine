{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module OpenGames.Engine.Interleaving where

-- import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.TLL
import OpenGames.Engine.OpenGames
import OpenGames.Engine.OpticClass
import OpenGames.Engine.Nat

interleave2 :: forall o c n m a a' b b' x x' y y' s s' r r'.
              (Optic o, Context c o, Show y, Show x, Show y', Show x')
           => OpenGame o c n a b (x, Maybe y') s y r
           -> OpenGame o c m a' b' (x', Maybe y) s' y' r'
           -> OpenGame o c (Add n m)(Flatten a a') (Flatten b b') (x, x') (s, s') (y, y') (r, r')
interleave2 g h = [opengame|
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

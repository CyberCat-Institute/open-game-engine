{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module OpenGames.Engine.Interleaving where

-- import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.TLL
import OpenGames.Engine.OpenGames
import OpenGames.Engine.OpticClass

interleave2 :: (Optic o, Context c o, Unappend a, Unappend a')
           => OpenGame o c a b (x, Maybe y') s y r
           -> OpenGame o c a' b' (x', Maybe y) s' y' r'
           -> OpenGame o c  (a +:+ a') (b +:+ b') (x, x') (s, s') (y, y') (r, r')
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

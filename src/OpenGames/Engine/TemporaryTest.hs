{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Engine.TemporaryTest where

import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGames
import OpenGames.Engine.Pure
import OpenGames.Preprocessor.CompileBlock

data PDMoves = Cooperate | Defect deriving (Show)

pdpayoffs :: PDMoves -> PDMoves -> (Double, Double)
pdpayoffs Cooperate Cooperate = (2, 2)
pdpayoffs Cooperate Defect    = (0, 3)
pdpayoffs Defect    Cooperate = (3, 0)
pdpayoffs Defect    Defect    = (1, 1)

pd = [opengame|
  operation : decision [Cooperate, Defect] ;
  outputs : x ;
  returns : fst (pdpayoffs x y) ;

  operation : decision [Cooperate, Defect] ;
  outputs : y ;
  returns : snd (pdpayoffs x y) ;
|]

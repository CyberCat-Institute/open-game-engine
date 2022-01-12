{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Engine.TemporaryTest where

import OpenGames.Engine.OpticClass
import OpenGames.Engine.OpenGames
import OpenGames.Engine.Pure
import OpenGames.Preprocessor.CompileBlock
{-
import OpenGames.Preprocessor.BlockSyntax
import OpenGames.Preprocessor.Codegen
import OpenGames.Preprocessor.CompileSyntax
import OpenGames.Preprocessor.Parser
import OpenGames.Preprocessor.RuntimeAST
-}

data PDMoves = Cooperate | Defect deriving (Show)

pdpayoffs :: PDMoves -> PDMoves -> (Double, Double)
pdpayoffs Cooperate Cooperate = (2, 2)
pdpayoffs Cooperate Defect    = (0, 3)
pdpayoffs Defect    Cooperate = (3, 0)
pdpayoffs Defect    Defect    = (1, 1)

pd = [opengame|
  operation : undefined ;
  outputs : x ;
  returns : fst (pdpayoffs x y) ;

  operation : undefined ;
  outputs : y ;
  returns : snd (pdpayoffs x y) ;
|]

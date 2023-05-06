{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import OpenGames.Engine.KleisliOptics
import Opengames.Compiler
import Opengames.Preprocessor

matchingPennies =
  [opengame|

    label : player1 ;
    operation : reindex const (decision "player1" [Heads, Tails]) ;
    outputs : x ;
    returns : matchingPenniesMatrix1 x $ y ;

    label : player2 ;
    operation : reindex const (decision "player2" [Heads, Tails]) ;
    outputs : y ;
    returns : matchingPenniesMatrix2 x y ;
|]

main :: IO ()
main = undefined

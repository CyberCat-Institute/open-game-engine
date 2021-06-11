{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.Generics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Compile
import Language.Haskell.TH.Syntax
import OpenGames.Engine.BayesianDiagnostics
import Numeric.Probability.Distribution
import qualified OpenGames.Engine.BayesianDiagnosticsTLL as TLL

data Coin = Heads | Tails deriving (Eq, Ord, Show)

matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Rational
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1

matchingPennies = [opengame|

    operation : reindex const (decision "player1" [Heads, Tails]) ;
    outputs : x ;
    returns : matchingPenniesMatrix1 x $ y ;

    operation : reindex const (decision "player2" [Heads, Tails]) ;
    outputs : y ;
    returns : matchingPenniesMatrix2 x y ;

|]

matchingPenniesEquilibrium = equilibrium matchingPennies trivialContext

main :: IO ()
main = do print "test"
          case (matchingPenniesEquilibrium (uniform [Heads, Tails], uniform [Heads, Tails])) of
            [] -> putStrLn "equlibrium!"
            x -> putStrLn "No equilibrium" *> print x





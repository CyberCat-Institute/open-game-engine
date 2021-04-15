{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenGames.Examples.Bimatrix where

import GHC.Generics
import OpenGames.Preprocessor.THSyntax
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Preprocessor.Compile
import Language.Haskell.TH.Syntax
import OpenGames.Engine.BayesianDiagnostics
import Numeric.Probability.Distribution
import qualified OpenGames.Engine.BayesianDiagnosticsTLL as TLL

-- Matching pennies

data Coin = Heads | Tails deriving (Eq, Ord, Show)

matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Rational
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1

-- Using TH
generateGame "matchingPenniesTH" []
                        [Line [] [] [|reindex const (decision "player1" [Heads, Tails])|] ["x"] [[|matchingPenniesMatrix1 x y|]],
                         Line [] [] [|reindex const (decision "player2" [Heads, Tails])|] ["y"] [[|matchingPenniesMatrix2 x y|]]]

--
-- Using Quasiquotes
matchingPennies = [opengame|

    operation : reindex const (decision "player1" [Heads, Tails]) ;
    outputs : x ;
    returns : matchingPenniesMatrix1 x $ y ;

    operation : reindex const (decision "player2" [Heads, Tails]) ;
    outputs : y ;
    returns : matchingPenniesMatrix2 x y ;

|]

matchingPenniesEquilibrium = equilibrium matchingPennies trivialContext


-- Meeting in New York

data Coordination = GCT | ES deriving (Eq, Ord, Show, Generic)

meetingInNYMatrix :: Coordination -> Coordination -> Rational
meetingInNYMatrix x y = if x == y then 1 else 0

-- Using TH
generateGame "meetingInNYTH" []
                        [Line [] [] [|reindex const (decision "player1" [GCT, ES])|] ["x"] [[|meetingInNYMatrix x y|]],
                         Line [] [] [|reindex const (decision "player2" [GCT, ES])|] ["y"] [[|meetingInNYMatrix x y|]]]

-- Using Quasiquotes
meetingInNY = [game| || =>>
  x | meetingInNYMatrix x y <- reindex const (decision "player1" [GCT, ES]) -< | ;
  y | meetingInNYMatrix x y <- reindex const (decision "player2" [GCT, ES]) -< | ;
  <<= ||
|]

meetingInNYEquilibrium = equilibrium meetingInNY trivialContext

-- Adding a third player

meetingInNYMatrix3 :: Coordination -> Coordination -> Coordination -> Rational
meetingInNYMatrix3 x y z = if x == y && x == z then 1 else 0

-- Using TH
generateGame "meetingInNY3TH" []
                        [Line [] [] [|reindex const (decision "player1" [GCT, ES])|] ["x"] [[|meetingInNYMatrix3 x y z|]],
                         Line [] [] [|reindex const (decision "player2" [GCT, ES])|] ["y"] [[|meetingInNYMatrix3 x y z|]],
                         Line [] [] [|reindex const (decision "player3" [GCT, ES])|] ["z"] [[|meetingInNYMatrix3 x y z|]]]

-- Using Quasiquotes
meetingInNY3 = [game| || =>>
  x | meetingInNYMatrix3 x y z <- reindex const (decision "player1" [GCT, ES]) -< | ;
  y | meetingInNYMatrix3 x y z <- reindex const (decision "player2" [GCT, ES]) -< | ;
  z | meetingInNYMatrix3 x y z <- reindex const (decision "player3" [GCT, ES]) -< | ;
   <<= ||
|]

meetingInNYEquilibrium3 = equilibrium meetingInNY3 trivialContext

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Examples.SequentialMoves where

import Engine.Engine
import Preprocessor.Preprocessor

-- A sequential version of rock paper scissors

-- 0. Types and payoffs

data ActionRPS = Rock | Paper | Scissors
   deriving (Eq,Ord,Show)

rockPaperScissorsMatrix :: ActionRPS -> ActionRPS -> Double
rockPaperScissorsMatrix Rock Scissors     = 1
rockPaperScissorsMatrix Rock Paper        = -1
rockPaperScissorsMatrix Rock Rock         = 0
rockPaperScissorsMatrix Scissors Rock     = -1
rockPaperScissorsMatrix Scissors Paper    = 1
rockPaperScissorsMatrix Scissors Scissors = 0
rockPaperScissorsMatrix Paper Rock        = 1
rockPaperScissorsMatrix Paper Scissors    = -1
rockPaperScissorsMatrix Paper Paper       = -1

-- | A sequential version of Rock Paper Scissors in verbose form
-- NOTE: Player 2 gets to observe the choice by player 1
rockPaperScissorsSeqVerbose = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Rock,Paper,Scissors]);
   outputs   : decisionPlayer1 ;
   returns   : rockPaperScissorsMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    : decisionPlayer1 ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Rock,Paper,Scissors]);
   outputs   : decisionPlayer2 ;
   returns   : rockPaperScissorsMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
   |]

-- | Same game in reduced form
rockPaperScissorsSeqReduced = [opengame|

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Rock,Paper,Scissors]);
   outputs   : decisionPlayer1 ;
   returns   : rockPaperScissorsMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    : decisionPlayer1 ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Rock,Paper,Scissors]);
   outputs   : decisionPlayer2 ;
   returns   : rockPaperScissorsMatrix decisionPlayer2 decisionPlayer1 ;

   |]



-- 1.  Analysis

-- | Equilibrium definition
isEquilibriumPrisonersDilemma strat = generateIsEq $ evaluate rockPaperScissorsSeqReduced  strat void


-- | Fix an arbitrary strategy by player 1
stratPlayer1 :: ActionRPS -> Kleisli Stochastic () ActionRPS 
stratPlayer1 x = pureAction x

-- | Optimal reply by player 2
optimalReplyPlayer2 Paper    = playDeterministically Scissors
optimalReplyPlayer2 Rock     = playDeterministically Paper
optimalReplyPlayer2 Scissors = playDeterministically Rock

-- | Fix the winning strategy
stratPlayer2 :: Kleisli Stochastic ActionRPS ActionRPS
stratPlayer2 = Kleisli $ optimalReplyPlayer2

-- | Define the strategy tuple for two players
stratTuple actionPlayer1 = stratPlayer1 actionPlayer1 ::- stratPlayer2 ::- Nil

-- Example usage
--  isEquilibriumPrisonersDilemma (stratTuple Rock)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Examples.SequentialMoves where

import OpenGames
import OpenGames.Preprocessor

--------------
-- 0. Overview
-- This file contains three simple sequential move games: the ultimatum game, the trust game, and a sequential version of rock paper scissors


-----------------------
-- 1. Types and payoffs

-- 1.0 Ultimatum Game

type Pie = Double
type Proposal = Double

data ResponderAction = Accept | Reject
  deriving (Eq,Ord,Show)

ultimatumGamePayoffProposer,ultimatumGamePayoffResponder :: Pie -> Proposal -> ResponderAction -> Payoff
ultimatumGamePayoffProposer pie proposal reaction =
  if reaction == Accept then pie - proposal else 0
ultimatumGamePayoffResponder pie proposal reaction =
  if reaction == Accept then proposal else 0

-- 1.1 Trust Game
type Sent = Double
type SentBack = Double
type Factor = Double

trustGamePayoffProposer,trustGamePayoffResponder :: Factor -> Sent -> SentBack -> Payoff
trustGamePayoffProposer factor send reaction = reaction
trustGamePayoffResponder factor proposal reaction = proposal * factor - reaction

-- 1.2. Sequential rockPaperScissors
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


--------------------
-- 1. Representation
-- 1.0. Ultimatum Game
ultimatumGame pie = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "proposer" (const [1..pie]);
   outputs   : proposal ;
   returns   : ultimatumGamePayoffProposer pie proposal reaction;

   inputs    : proposal ;
   feedback  :      ;
   operation : dependentDecision "responder" (const [Accept,Reject]);
   outputs   : reaction ;
   returns   : ultimatumGamePayoffResponder pie proposal reaction ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
   |]

-- 1.1. Trust Game
trustGame pie factor = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :   ;
   feedback  :      ;
   operation : dependentDecision "proposer" (const [1..pie]);
   outputs   : sent ;
   returns   : trustGamePayoffProposer factor sent sentBack;

   inputs    : sent;
   feedback  :      ;
   operation : dependentDecision "responder" (\x -> [0,x]);
   outputs   : sentBack ;
   returns   : trustGamePayoffResponder factor sent sentBack ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
   |]


-- 1.2. A sequential version of Rock Paper Scissors in verbose form
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



---------------
-- 2.  Analysis
-- 2.0. Ultimatum Game
isEquilibriumUltimatumGame pie strat = generateIsEq $ evaluate (ultimatumGame pie) strat void

-- proposer plays selfish
proposerSelfishStrategyUG :: Kleisli Stochastic () Proposal
proposerSelfishStrategyUG = pureAction 1

-- responder accepts all proposals
responderStrategyUG :: Kleisli Stochastic Proposal ResponderAction
responderStrategyUG = pureAction Accept

-- illustration of another responder strategy
responderStrategy' :: Kleisli Stochastic Proposal ResponderAction
responderStrategy' = Kleisli acceptThreshold
   where
    acceptThreshold :: Proposal -> Stochastic ResponderAction
    acceptThreshold proposal = if proposal > 5 then playDeterministically Accept else uniformDist [Accept,Reject]


-- strategy Tuple
stratTupleUG = proposerSelfishStrategyUG ::- responderStrategyUG ::- Nil

-- Example usage
-- isEquilibriumUltimatumGame 10 stratTupleUG

-- 2.1. Trust Game
isEquilibriumTrustGame pie factor strat = generateIsEq $ evaluate (trustGame pie factor) strat void

-- proposer plays selfish
proposerSelfishStrategyTG :: Kleisli Stochastic () Sent
proposerSelfishStrategyTG = pureAction 1

-- responder sends back nothing
responderSelfishStrategyTG :: Kleisli Stochastic Sent SentBack
responderSelfishStrategyTG = pureAction 0


-- strategy Tuple
stratTupleTG = proposerSelfishStrategyTG ::- responderSelfishStrategyTG ::- Nil

-- Example usage
-- isEquilibriumTrustGame 10 2 stratTupleTG

-- 2.2. Sequential Rock Paper Scissors

-- | Equilibrium definition
isEquilibriumRPSSeq strat = generateIsEq $ evaluate rockPaperScissorsSeqReduced  strat void

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
stratTupleRPSSSeq actionPlayer1 = stratPlayer1 actionPlayer1 ::- stratPlayer2 ::- Nil

-- Example usage
--  isEquilibriumRPSSSeq (stratTuple Rock)

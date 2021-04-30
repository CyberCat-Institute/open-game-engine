{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.SimultaneousMoves where


import Engine.Engine
import Preprocessor.Preprocessor


--------------
-- 0. Overview
-- This file contains three simple simultaneous move games: prisoner dilemma (a social dilemma), meeting in new york (coordination game), and matching pennies (anti-coordination game)



-----------------------
-- 1. Types and payoffs

-- 1.0. Prisoner's dilemma
data ActionPD = Cooperate | Defect
  deriving (Eq, Ord, Show)

-- | Payoff matrix for player i given i's action and j's action
prisonersDilemmaMatrix :: ActionPD -> ActionPD -> Double
prisonersDilemmaMatrix Cooperate Cooperate   = 3
prisonersDilemmaMatrix Cooperate Defect  = 0
prisonersDilemmaMatrix Defect Cooperate  = 5
prisonersDilemmaMatrix Defect Defect = 1


-- 1.1. Meeting in New York
data Location = EmpireState | GrandCentral deriving (Eq, Ord, Show)

-- | Payoff matrix for player i and j
meetingInNYMatrix :: Location -> Location -> Double
meetingInNYMatrix x y = if x == y then 1 else 0

-- 1.2. Matching pennies
data Coin = Heads | Tails
   deriving (Eq, Ord, Show)

-- | Payoff matrix for player 1 and player 2
-- NOTE: We use two functions here as payoffs are asymmetric
-- This results in differences how we use payoffs in the game definition below
matchingPenniesMatrix1, matchingPenniesMatrix2 :: Coin -> Coin -> Double
matchingPenniesMatrix1 x y = if x == y then 1 else 0
matchingPenniesMatrix2 x y = if x == y then 0 else 1


--------------------
-- 1. Representation
-- 1.0 Prisoner's dilemma

-- | Prisoner's dilemma in verbose form
-- NOTE there are no ingoing or outgoing arrows
-- This is a feature that any representation of a classical game shares
-- NOTE the switch of the payoff 
prisonersDilemmaVerbose = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]


-- | Same game as above but without empty fields
prisonersDilemmaReduced = [opengame|

   operation : dependentDecision "player1" (const [Cooperate,Defect]);
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   operation : dependentDecision "player2" (const [Cooperate,Defect]);
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

  |]

-- 1.1 Meeting in New York
meetingInNYReduced = [opengame|

   operation : dependentDecision "player1" (const [EmpireState,GrandCentral]);
   outputs   : decisionPlayer1 ;
   returns   : meetingInNYMatrix decisionPlayer1 decisionPlayer2 ;

   operation : dependentDecision "player2" (const [EmpireState,GrandCentral]);
   outputs   : decisionPlayer2 ;
   returns   : meetingInNYMatrix decisionPlayer2 decisionPlayer1 ;

  |]

-- 1.2. Matching pennies
matchingPenniesReduced = [opengame|

    operation : dependentDecision "player1" (const [Heads, Tails]) ;
    outputs   : decisionPlayer1 ;
    returns   : matchingPenniesMatrix1 decisionPlayer1 decisionPlayer2 ;

    operation : dependentDecision "player1" (const [Heads, Tails]) ;
    outputs   : decisionPlayer2 ;
    returns   : matchingPenniesMatrix2 decisionPlayer2 decisionPlayer1 ;

|]

--------------------------
-- 2. Equilibrium analysis
-- We provide ways to evaluate the games as well as example strategies which are in equilibrium

-- 2.0. Prisoner's dilemma
-- | Evaluate the prisoner's dilemma
-- This function expects a strategy tuple and checks whether this strategy is in equilibrium
-- NOTE As for the other games, we give a _void_ context to the _evaluate_ function as there is no ingoing and outgoing information flow
isEquilibriumPrisonersDilemma strat = generateIsEq $ evaluate prisonersDilemmaReduced strat void

-- | Define pure single player strategies
cooperateStrategy :: Kleisli Stochastic () ActionPD
cooperateStrategy = pureAction Cooperate
-- ^ play _Cooperate_ with certainty
defectStrategy :: Kleisli Stochastic () ActionPD
defectStrategy = pureAction Defect
-- ^ play _Defect_ with certainty

-- | Combine single player's strategies into a tuple
strategTupleCooperate = cooperateStrategy ::- cooperateStrategy ::- Nil
-- ^ Both players cooperate with certainty
strategTupleDefect = defectStrategy ::- defectStrategy ::- Nil
-- ^ Both players defect with certainty

-- isEquilibriumPrisonersDilemma strategTupleCooperate -- NOT an eq
-- isEquilibriumPrisonersDilemma strategTupleDefect -- eq


-- 2.1 Meeting in New York
-- | Evaluate the meeting in New York game
isEquilibriumMeetingInNY strat = generateIsEq $ evaluate meetingInNYReduced strat void

-- | Define pure single player strategies
empireStateStrategy  :: Kleisli Stochastic () Location
empireStateStrategy  = pureAction EmpireState
-- ^ play _EmpireState_ with certainty
grandCentralStrategy :: Kleisli Stochastic () Location
grandCentralStrategy = pureAction GrandCentral
-- ^ play _GrandCentral_ with certainty

-- | Combine single player's strategies into a tuple
strategyTupleEmpireState  = empireStateStrategy ::- empireStateStrategy ::- Nil
-- ^ Both players meet at EmpireState with certainty
strategyTupleGrandCentral = grandCentralStrategy ::- grandCentralStrategy ::- Nil
-- ^ Both players meet at EmpireState with certainty
strategyTupleGrandAndEmpire = grandCentralStrategy ::- empireStateStrategy ::- Nil
-- ^ Player 1 meets at grand central and player 2 meets at empire state 

-- isEquilibriumMeetingInNY strategyTupleGrandAndEmpire - NOT eq
-- isEquilibriumMeetingInNY strategyTupleEmpireState - eq
-- isEquilibriumMeetingInNY strategyTupleGrandCentral - eq


-- 2.2 Matching Pennies
-- | Evaluate the meeting in New York game
isEquilibriumMatchingPennies strat = generateIsEq $ evaluate matchingPenniesReduced strat void

-- | Define pure single player strategies
headsStrategy  :: Kleisli Stochastic () Coin
headsStrategy  = pureAction Heads
-- ^ play _Heads_ with certainty

tailsStrategy :: Kleisli Stochastic () Coin
tailsStrategy = pureAction Tails
-- ^ play _Tails_ with certainty

-- | Define MIXED single player strategy
uniformActionDist = uniformDist [Heads,Tails]
-- ^ Define the uniform distribution on action space

mixedStrategy = Kleisli $ const uniformActionDist
-- ^ Define proper mixed strategy 

-- | Combine single player's strategies into a tuple
strategyTupleHeads  = headsStrategy ::- headsStrategy ::- Nil
-- ^ Both players meet at EmpireState with certainty
strategyTupleTails = tailsStrategy ::- tailsStrategy ::- Nil
-- ^ Both players meet at EmpireState with certainty
strategyTupleMixed = mixedStrategy ::- mixedStrategy ::- Nil
-- ^ Both players choose both action with equal probability 

-- isEquilibriumMatchingPennies strategyTupleHeads - NOT an eq
-- isEquilibriumMatchingPennies strategyTupleTails - NOT an eq
-- isEquilibriumMatchingPennies strategyTupleMixed - is an eq

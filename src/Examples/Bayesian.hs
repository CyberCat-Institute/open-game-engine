{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Examples.Bayesian where

import OpenGames
import OpenGames.Preprocessor


--------------
-- 0. Overview
-- This file contains two examples. First, a single player making a decision updating information given a signal structure. This games illustrates the Bayesian updating inherent in the players' optimization. Second, a Bayesian prisoner's dilemma, taken from Mas-Colell, Whinston & Green p.254, this is illustrates the interaction of a Bayesian player with private information and a player without private information in the setting of a prisoner's dilemma.

-----------------------
-- 1. Types and payoffs

-- 1.0 Single player coordinating with nature

data CoordinateMove = A | B deriving (Eq,Ord,Show)

-- | payoff for player is matching the right state of the world
coordinateWithNaturePayoff :: CoordinateMove -> CoordinateMove -> Double
coordinateWithNaturePayoff x y = if x == y then 1 else 0

-- 1.1 Prisoner's dilemma
data PDNature = Rat | NoRat deriving (Eq, Ord, Show)
data PDMove = Confess | DontConfess deriving (Eq, Ord, Show)

-- | standard PD payoff
pdPayoff1 :: PDMove -> PDMove -> Double
pdPayoff1 Confess Confess = -5
pdPayoff1 Confess DontConfess = -1
pdPayoff1 DontConfess Confess = -10
pdPayoff1 DontConfess DontConfess = 0

-- | different payoffs depending on player type ("Rat" - "NoRat")
pdPayoff2 :: PDNature -> PDMove -> PDMove -> Double
pdPayoff2 Rat Confess Confess = -5
pdPayoff2 Rat Confess DontConfess = -10
pdPayoff2 Rat DontConfess Confess = -1
pdPayoff2 Rat DontConfess DontConfess = -2
pdPayoff2 NoRat Confess Confess = -11
pdPayoff2 NoRat Confess DontConfess = -10
pdPayoff2 NoRat DontConfess Confess = -7
pdPayoff2 NoRat DontConfess DontConfess = -2


--------------------------
-- 2. Game representations
-- 2.0. coordination with nature

-- | Distribution of states of the world
distributionNature probA = distFromList [(A,probA),(B, 1- probA)]

-- | signal structure, given precision probability that the correct state of nature is sent
signal signalPrecision A = distFromList [(A,signalPrecision),(B, 1- signalPrecision)]
signal signalPrecision B = distFromList [(B,signalPrecision),(A, 1- signalPrecision)]

-- | This game represents a stochastic process: Nature draws from a distribution given _probA_ of A, then a signal is sent with a given probability _signalPrecision_
stochasticEnv probA signalPrecision = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : nature (distributionNature probA);
   outputs   : draw ;
   returns   :      ;

   inputs    : draw     ;
   feedback  :      ;
   operation : liftStochasticForward (signal signalPrecision);
   outputs   : signalDraw ;
   returns   :      ;
   :----------------------------:

   outputs   :  (signalDraw,draw) ;
   returns   :      ;
  |]


-- | The complete game
coordinateWithNature probA signalPrecision = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : stochasticEnv probA signalPrecision ;
   outputs   : (signal,draw) ;
   returns   : ;

   inputs    : signal ;
   feedback  :      ;
   operation : dependentDecision "player" (const [A,B]);
   outputs   : decision ;
   returns   : coordinateWithNaturePayoff decision draw;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
   |]

-- 2.1. Prisoner's dilemma
bayesianPD  = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : nature (uniformDist [Rat, NoRat]) ;
   outputs   : prisoner2Type ;
   returns   : ;

   inputs    :  ;
   feedback  :      ;
   operation : dependentDecision "prisoner1" (const [Confess, DontConfess]);
   outputs   : decision1 ;
   returns   : pdPayoff1 decision1 decision2;

   inputs    : prisoner2Type ;
   feedback  :      ;
   operation : dependentDecision "prisoner2" (const [Confess, DontConfess]);
   outputs   : decision2 ;
   returns   : pdPayoff2 prisoner2Type decision1 decision2 ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
   |]

--------------------------
-- 3. Equilibrium analysis

-- 3.0. Coordinate with nature

-- | Equilibrium function
isEquilibriumCoordinationNature probA signalPrecision strat = generateIsEq $ evaluate (coordinateWithNature probA signalPrecision) strat void

-- | We define two strategies _followSignal_ and _doOpposite_
followSignal,doOpposite :: Kleisli Stochastic CoordinateMove CoordinateMove
followSignal = Kleisli (\x -> playDeterministically x)
doOpposite   = Kleisli (\case {A -> playDeterministically B; B -> playDeterministically A})

-- | Putting strategies into the list
followSignalStrategy = followSignal ::- Nil
doOppositeStrategy   = doOpposite ::- Nil

-- Example usage
-- isEquilibriumCoordinationNature 0.6 0.7 followSignalStrategy

-- 3.1. Bayesian Prisoner's Dilemma

-- | Equilibrium function
isEquilibriumBayesianPDE strat  = generateIsEq $  evaluate bayesianPD strat void


-- | Strategy player 1
player1Strategy :: Kleisli Stochastic () PDMove
player1Strategy = pureAction Confess

-- | Strategy player 2
player2Strategy :: Kleisli Stochastic PDNature PDMove
player2Strategy  = Kleisli (\case { Rat   -> playDeterministically Confess
                                  ; NoRat -> playDeterministically DontConfess})

-- | Complete strategy tuple
strategyTuplePD = player1Strategy ::- player2Strategy ::- Nil

-- Example usage
-- isEquilibriumBayesianPDE strategyTuplePD


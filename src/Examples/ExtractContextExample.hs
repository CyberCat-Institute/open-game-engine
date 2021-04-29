{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}


module Examples.ExtractContextExample where

import           Data.Aeson
import           Graphics.Vega.VegaLite
import           GHC.Generics

import Engine.Engine
import Preprocessor.Preprocessor


--------------
-- 0. Overview
-- This file contains three simple simultaneous move games: prisoner dilemma (a social dilemma), meeting in new york (coordination game), and matching pennies (anti-coordination game)



-- 1.Meeting in New York
data Location = EmpireState | GrandCentral deriving (Eq, Ord, Show,Generic)

instance ToJSON Location

-- | Payoff matrix for player i and j
meetingInNYMatrix :: Location -> Location -> Double
meetingInNYMatrix x y = if x == y then 1 else 0

-- 1.1 Meeting in New York
meetingInNYReduced = [opengame|

   operation : dependentDecision "player1" (const [EmpireState,GrandCentral]);
   outputs   : decisionPlayer1 ;
   returns   : meetingInNYMatrix decisionPlayer1 decisionPlayer2 ;

   operation : dependentDecision "player2" (const [EmpireState,GrandCentral]);
   outputs   : decisionPlayer2 ;
   returns   : meetingInNYMatrix decisionPlayer2 decisionPlayer1 ;

  |]


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

testNY = evaluate meetingInNYReduced strategyTupleGrandAndEmpire void

testContext :: [(String, Location -> Double)]
testContext = fromIndex Zero  (generateContext testNY)


deviationsPlayer1 = snd $ head  [(s,f) | (s,f) <- testContext, s == "player1"]

data ExportContext = ExportContext
   { action  :: !Location
   , payoff  :: !Double
   } deriving (Generic,Show)

instance ToJSON ExportContext


exportData = [(ExportContext EmpireState (deviationsPlayer1 EmpireState)), (ExportContext GrandCentral (deviationsPlayer1 GrandCentral))]

exportContext :: Value
exportContext = toJSONList [(ExportContext EmpireState (deviationsPlayer1 EmpireState)), (ExportContext GrandCentral (deviationsPlayer1 GrandCentral))]

-- export vega

meetingNYPlayer1 :: VegaLite
meetingNYPlayer1 = 
     let enc = encoding
              . position X [ PName "action", PmType Nominal ]
              . position Y [ PName "payoff", PmType Quantitative ]
         contextData = dataFromJson exportContext []

     in toVegaLite
         [ contextData
         , mark Bar []
         , enc []
         , height 300
         , width  200
         , title "Unilateral Context for Player 1" []
         ]

module Main where

import Examples.Decision
import Examples.SimultaneousMoves
import Examples.SequentialMoves


main = do
  putStrLn "Single decision -->"
  isOptimalSingleDecisionVerbose (pureIdentity 4)
  putStrLn "\n Single decision -->"
  isOptimalSingleDecisionVerbose (pureIdentity 5)
  putStrLn "\n Single decision with stochastic environment -->"
  isOptimalSingleDecisionStoch peak
  putStrLn "\n Prisoner's dilemma both cooperate -->"
  isEquilibriumPrisonersDilemma strategTupleCooperate
  putStrLn "\n Prisoner's dilemma both defect -->"
  isEquilibriumPrisonersDilemma strategTupleDefect
  putStrLn "\n Meeting in New York diff location -->"
  isEquilibriumMeetingInNY strategyTupleGrandAndEmpire
  putStrLn "\n Meeting in New York both ES -->"
  isEquilibriumMeetingInNY strategyTupleEmpireState
  putStrLn "\n Matching Pennies - mixed with equal prob -->"
  isEquilibriumMatchingPennies strategyTupleMixed

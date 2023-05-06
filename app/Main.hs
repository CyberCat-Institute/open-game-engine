module Main where

import Examples.Decision
import Examples.SequentialMoves
import Examples.SimultaneousMoves

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
  putStrLn "\n Matching Pennies - mixed with equal prob -->"
  isEquilibriumMatchingPennies strategyTupleMixed

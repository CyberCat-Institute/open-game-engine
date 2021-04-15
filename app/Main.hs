module Main where

import Examples.Decision


main = do
  putStrLn "Single decision -->"
  isOptimalSingleDecisionVerbose (pureIdentity 4)
  putStrLn "Single decision -->"
  isOptimalSingleDecisionVerbose (pureIdentity 5)
  putStrLn "Single decision with stochastic environment -->"
  isOptimalSingleDecisionStoch peak


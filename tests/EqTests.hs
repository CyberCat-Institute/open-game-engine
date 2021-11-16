module EqTests where

import Test.Hspec
import OpenGames.Examples.Consensus.TestPayoffs

tests :: IO ()
tests = do 
  hspec $ 
    it "should carry the state correctly" $  
      show (testEq testStrategy) `shouldBe` "[DiagnosticInfo {player = \"player1\", state = \"()\", unobservableState = \"((),())\", strategy = \"fromFreqs [(Cooperate,1.0)]\", payoff = \"6.0\", optimalMove = \"Defect\", optimalPayoff = \"8.0\"},DiagnosticInfo {player = \"player2\", state = \"()\", unobservableState = \"((),Cooperate)\", strategy = \"fromFreqs [(Cooperate,1.0)]\", payoff = \"6.0\", optimalMove = \"Defect\", optimalPayoff = \"8.0\"},DiagnosticInfo {player = \"player1\", state = \"()\", unobservableState = \"((),(Cooperate,Cooperate))\", strategy = \"fromFreqs [(Cooperate,1.0)]\", payoff = \"3.0\", optimalMove = \"Defect\", optimalPayoff = \"5.0\"},DiagnosticInfo {player = \"player2\", state = \"()\", unobservableState = \"((),(Cooperate,Cooperate,Cooperate))\", strategy = \"fromFreqs [(Cooperate,1.0)]\", payoff = \"3.0\", optimalMove = \"Defect\", optimalPayoff = \"5.0\"}]"


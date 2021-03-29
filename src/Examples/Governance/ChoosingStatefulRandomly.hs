module Examples.Governance.ChoosingStatefulRandomly where

import           Numeric.Probability.Distribution
import           Engine.StatefulBayesian
import           Preprocessor.AbstractSyntax

type Player = String

selection :: Stochastic (Player, Player)
selection = uniform [("player1", "player2"), ("player1", "player3"), ("player2", "player3")]

randomPlayersSrc = Block [] []
  [Line [] [] "liftStochastic selection" ["role1", "role2"] []]
  [] []

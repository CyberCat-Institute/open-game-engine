module OpenGames.Examples.Governance.ChoosingStatefulRandomly where

import           Numeric.Probability.Distribution
import           OpenGames.Engine.StatefulBayesian
import           OpenGames.Preprocessor.AbstractSyntax

type Player = String

selection :: Stochastic (Player, Player)
selection = uniform [("player1", "player2"), ("player1", "player3"), ("player2", "player3")]

randomPlayersSrc = Block [] []
  [Line Nothing [] [] "liftStochastic selection" ["role1", "role2"] []]
  [] []

module OpenGames.Examples.Governance.VotingGames where

import           Control.Arrow (Kleisli(..))
import           Data.List
import           Numeric.Probability.Distribution


import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian
import           OpenGames.Preprocessor.AbstractSyntax


-- 0 Stage games -----
    -- 0. Implement UG
    -- 1. Implement Trust Game

-- 1 Voting stage
    -- 0. Random choice of games
    -- 1. Refactor this stage to include voting (simple majority and quadractic)

-- 2 Refactor stage game
   -- 0. Add Voting into roles
   -- 1. Voting into concrete gambles

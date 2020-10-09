module OpenGames.Examples.Governance.VotingGames where

import           Control.Arrow (Kleisli(..))
import           Data.List
import           Numeric.Probability.Distribution


import           OpenGames.Engine.OpenGamesClass
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.StatefulBayesian hiding (roleDecision, dependentDecision)
import           OpenGames.Engine.DependentDecision
import           OpenGames.Preprocessor.AbstractSyntax

--------------------------
-- 0. Types and parameters

type Transfer = Double
type Endowment = Double

endowment :: Endowment
endowment = 10

tgFactor :: Double
tgFactor = 3




payoffUG :: Endowment ->  Transfer -> Bool -> (Double, Double)
payoffUG _   _     False = (0,0)
payoffUG end trans True  = (end - trans, trans)

payoffTG :: Endowment ->  Transfer -> Transfer -> Transfer -> (Double, Double)
payoffTG end trans received return  = (end - trans + return, received - return)


--------------------------
-- 1. Stage games

ultimatumGameSrc = Block ["sender" :: Agent, "receiver" :: Agent] []
   [Line ["(sender,())"] [] "roleDecision  [0..endowment]" ["amountProposed"] ["fst $ payoffUG endowment amountProposed accDec" ],
    Line ["(receiver,amountProposed)"] [] "roleDecision [True,False]" ["accDec"] ["snd $ payoffUG endowment amountProposed accDec"]
   ]
   [] []

ultimatumGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(sender, receiver, amountProposed, accDec) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sender, receiver) -> ((sender, receiver), (sender,()))) (\((sender, receiver, amountProposed, accDec), ()) -> (sender, receiver, amountProposed, accDec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision  [0..endowment])))))) >>> (fromFunctions (\((sender, receiver), amountProposed) -> (sender, receiver, amountProposed)) (\(sender, receiver, amountProposed, accDec) -> ((sender, receiver, amountProposed, accDec), fst $ payoffUG endowment amountProposed accDec))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sender, receiver, amountProposed) -> ((sender, receiver, amountProposed), (receiver,amountProposed))) (\((sender, receiver, amountProposed, accDec), ()) -> (sender, receiver, amountProposed, accDec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [True,False])))))) >>> (fromFunctions (\((sender, receiver, amountProposed), accDec) -> (sender, receiver, amountProposed, accDec)) (\(sender, receiver, amountProposed, accDec) -> ((sender, receiver, amountProposed, accDec), snd $ payoffUG endowment amountProposed accDec))))))))) >>> (fromLens (\(sender, receiver, amountProposed, accDec) -> ()) (curry (\((sender, receiver, amountProposed, accDec), ()) -> (sender, receiver, amountProposed, accDec)))))



trustGameSrc = Block ["sender" :: Agent, "receiver" :: Agent] []
   [Line ["(sender,())"] [] "roleDecision  [0..endowment]" ["amountProposed"] ["fst $ payoffTG endowment amountProposed (amountProposed * tgFactor) return" ],
    Line ["(receiver,amountProposed)"] [] "dependentRoleDecision (\\x -> [0..(x * tgFactor)])" ["return"] ["snd $ payoffTG endowment amountProposed (amountProposed * tgFactor) return"]
   ]
   [] []


trustGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(sender, receiver, amountProposed, return) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sender, receiver) -> ((sender, receiver), (sender,()))) (\((sender, receiver, amountProposed, return), ()) -> (sender, receiver, amountProposed, return))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision  [0..endowment])))))) >>> (fromFunctions (\((sender, receiver), amountProposed) -> (sender, receiver, amountProposed)) (\(sender, receiver, amountProposed, return) -> ((sender, receiver, amountProposed, return), fst $ payoffTG endowment amountProposed (amountProposed * tgFactor) return))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sender, receiver, amountProposed) -> ((sender, receiver, amountProposed), (receiver,amountProposed))) (\((sender, receiver, amountProposed, return), ()) -> (sender, receiver, amountProposed, return))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentRoleDecision (\x -> [0..(x * tgFactor)]))))))) >>> (fromFunctions (\((sender, receiver, amountProposed), return) -> (sender, receiver, amountProposed, return)) (\(sender, receiver, amountProposed, return) -> ((sender, receiver, amountProposed, return), snd $ payoffTG endowment amountProposed (amountProposed * tgFactor) return))))))))) >>> (fromLens (\(sender, receiver, amountProposed, return) -> ()) (curry (\((sender, receiver, amountProposed, return), ()) -> (sender, receiver, amountProposed, return)))))


--------------------------
-- 2. Voting on game

majorityGame :: (Fractional prob) => Either () () -> Either () () -> T prob (Either () ())
majorityGame (Left ()) (Left ()) = certainly (Left ())
majorityGame (Right ()) (Right ()) = certainly (Right ())
majorityGame _ _ = uniform [Left (), Right ()]

votingGameSrc = Block [] []
                    [Line [] [] "dependentDecision \"player1\" [Left (), Right ()])" ["vote1"] [],
                     Line [] [] "dependentDecision \"player2\" [Left (), Right ()])" ["vote2"] [],
                     Line ["vote1", "vote2"] [] "liftStochastic (uncurry majority2)" ["result"] [],
                     Line [] []  [] [],
                     Line ["result"] ["payoff1", "payoff2"] "prisonersDilemma +++ stagHunt" ["discard"] []]
                    [] []
votingGame = undefined --reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2, result, discard, payoff1, payoff2), ()) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player1" [Left (), Right ()]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ((vote1, vote2, result, discard, payoff1, payoff2), payoff1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2, result, discard, payoff1, payoff2), ()) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((reindex const (decision "player2" [Left (), Right ()]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ((vote1, vote2, result, discard, payoff1, payoff2), payoff2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2) -> ((vote1, vote2), (vote1, vote2))) (\((vote1, vote2, result, discard, payoff1, payoff2), ()) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((liftStochastic (uncurry majority2))))))) >>> (fromFunctions (\((vote1, vote2), result) -> (vote1, vote2, result)) (\(vote1, vote2, result, discard, payoff1, payoff2) -> ((vote1, vote2, result, discard, payoff1, payoff2), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2, result) -> ((vote1, vote2, result), result)) (\((vote1, vote2, result, discard), (payoff1, payoff2)) -> (vote1, vote2, result, discard, payoff1, payoff2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((prisonersDilemma +++ stagHunt)))))) >>> (fromFunctions (\((vote1, vote2, result), discard) -> (vote1, vote2, result, discard)) (\(vote1, vote2, result, discard) -> ((vote1, vote2, result, discard), ()))))))))) >>> (fromLens (\(vote1, vote2, result, discard) -> ()) (curry (\((vote1, vote2, result, discard), ()) -> (vote1, vote2, result, discard)))))


--------------------------
-- 3. Voting on role






-- 0 Stage games -----
    -- 0. Implement UG
    -- 1. Implement Trust Game

-- 1 Voting stage
    -- 0. Random choice of games
    -- 1. Refactor this stage to include voting (simple majority and quadractic)

-- 2 Refactor stage game
   -- 0. Add Voting into roles
   -- 1. Voting into concrete gambles

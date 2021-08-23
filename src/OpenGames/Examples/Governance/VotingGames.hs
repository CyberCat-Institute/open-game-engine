module OpenGames.Examples.Governance.VotingGames where

import           Control.Arrow (Kleisli(..))
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


uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c


payoffUG :: Endowment ->  Transfer -> Bool -> (Double, Double)
payoffUG _   _     False = (0,0)
payoffUG end trans True  = (end - trans, trans)

payoffTG :: Endowment ->  Transfer -> Transfer -> Transfer -> (Double, Double)
payoffTG end trans received return  = (end - trans + return, received - return)


--------------------------
-- 1. Stage games

ultimatumGameSrc = Block ["(sender, receiver)"] []
   [Line Nothing ["(sender,())"] [] "roleDecision  [0..endowment]" ["amountProposed"] ["fst $ payoffUG endowment amountProposed accDec" ],
    Line Nothing ["(receiver,amountProposed)"] [] "roleDecision [True,False]" ["accDec"] ["snd $ payoffUG endowment amountProposed accDec"]
   ]
   [] []

ultimatumGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\((sender, receiver), amountProposed, accDec) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sender, receiver) -> ((sender, receiver), (sender,()))) (\(((sender, receiver), amountProposed, accDec), ()) -> ((sender, receiver), amountProposed, accDec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision  [0..endowment])))))) >>> (fromFunctions (\((sender, receiver), amountProposed) -> ((sender, receiver), amountProposed)) (\((sender, receiver), amountProposed, accDec) -> (((sender, receiver), amountProposed, accDec), fst $ payoffUG endowment amountProposed accDec))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((sender, receiver), amountProposed) -> (((sender, receiver), amountProposed), (receiver,amountProposed))) (\(((sender, receiver), amountProposed, accDec), ()) -> ((sender, receiver), amountProposed, accDec))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision [True,False])))))) >>> (fromFunctions (\(((sender, receiver), amountProposed), accDec) -> ((sender, receiver), amountProposed, accDec)) (\((sender, receiver), amountProposed, accDec) -> (((sender, receiver), amountProposed, accDec), snd $ payoffUG endowment amountProposed accDec))))))))) >>> (fromLens (\((sender, receiver), amountProposed, accDec) -> ()) (curry (\(((sender, receiver), amountProposed, accDec), ()) -> ((sender, receiver), amountProposed, accDec)))))



trustGameSrc = Block ["(sender, receiver)"] []
   [Line Nothing ["(sender,())"] [] "roleDecision  [0..endowment]" ["amountProposed"] ["fst $ payoffTG endowment amountProposed (amountProposed * tgFactor) return" ],
    Line Nothing ["(receiver,amountProposed)"] [] "dependentRoleDecision (\\x -> [0..(x * tgFactor)])" ["return"] ["snd $ payoffTG endowment amountProposed (amountProposed * tgFactor) return"]
   ]
   [] []


trustGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\((sender, receiver), amountProposed, return) -> ())) >>> (reindex (\(a1, a2) -> (a1, a2)) ((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(sender, receiver) -> ((sender, receiver), (sender,()))) (\(((sender, receiver), amountProposed, return), ()) -> ((sender, receiver), amountProposed, return))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((roleDecision  [0..endowment])))))) >>> (fromFunctions (\((sender, receiver), amountProposed) -> ((sender, receiver), amountProposed)) (\((sender, receiver), amountProposed, return) -> (((sender, receiver), amountProposed, return), fst $ payoffTG endowment amountProposed (amountProposed * tgFactor) return))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\((sender, receiver), amountProposed) -> (((sender, receiver), amountProposed), (receiver,amountProposed))) (\(((sender, receiver), amountProposed, return), ()) -> ((sender, receiver), amountProposed, return))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentRoleDecision (\x -> [0..(x * tgFactor)]))))))) >>> (fromFunctions (\(((sender, receiver), amountProposed), return) -> ((sender, receiver), amountProposed, return)) (\((sender, receiver), amountProposed, return) -> (((sender, receiver), amountProposed, return), snd $ payoffTG endowment amountProposed (amountProposed * tgFactor) return))))))))) >>> (fromLens (\((sender, receiver), amountProposed, return) -> ()) (curry (\(((sender, receiver), amountProposed, return), ()) -> ((sender, receiver), amountProposed, return)))))


--------------------------
-- 2. Voting on game


-- NOTE the majorityfunction defaults to _Left_ (trustGame)
majorityGame :: (Agent,Agent) -> Either () () -> Either () () -> Either (Agent,Agent) (Agent,Agent)
majorityGame (ag1,ag2) (Left ()) (Left ()) = Left (ag1,ag2)
majorityGame (ag1,ag2) (Right ()) (Right ()) = Right (ag1,ag2)
majorityGame (ag1,ag2) _ _ = Left (ag1,ag2)

-- Voting on the game; roles are random
votingGameSrc = Block [] []
                    [Line Nothing [] [] "dependentDecision \"player1\" (const [Left (), Right ()])" ["vote1"] ["0"],
                     Line Nothing [] [] "dependentDecision \"player2\" (const [Left (), Right ()])" ["vote2"] ["0"],
                     Line Nothing [] [] "nature (uniform [(\"player1\",\"player2\"),(\"player2\",\"player2\")])" ["roles"] [],
                     Line Nothing ["majorityGame roles vote1 vote2"] [] "trustGame +++ ultimatumGame" ["discard"] []]
                    [] []
votingGame = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2, roles, discard) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2, roles, discard), ()) -> (vote1, vote2, roles, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "player1" (const [Left (), Right ()]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2, roles, discard) -> ((vote1, vote2, roles, discard), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2, roles, discard), ()) -> (vote1, vote2, roles, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "player2" (const [Left (), Right ()]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2, roles, discard) -> ((vote1, vote2, roles, discard), 0)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2) -> ((vote1, vote2), ())) (\((vote1, vote2, roles, discard), ()) -> (vote1, vote2, roles, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((nature (uniform [("player1","player2"),("player2","player2")]))))))) >>> (fromFunctions (\((vote1, vote2), roles) -> (vote1, vote2, roles)) (\(vote1, vote2, roles, discard) -> ((vote1, vote2, roles, discard), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2, roles) -> ((vote1, vote2, roles), majorityGame roles vote1 vote2)) (\((vote1, vote2, roles, discard), ()) -> (vote1, vote2, roles, discard))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((trustGame +++ ultimatumGame)))))) >>> (fromFunctions (\((vote1, vote2, roles), discard) -> (vote1, vote2, roles, discard)) (\(vote1, vote2, roles, discard) -> ((vote1, vote2, roles, discard), ()))))))))) >>> (fromLens (\(vote1, vote2, roles, discard) -> ()) (curry (\((vote1, vote2, roles, discard), ()) -> (vote1, vote2, roles, discard)))))



--------------------------
-- 3. Voting role in trust game

votingRole :: (Agent,Agent) -> Stochastic (Agent,Agent)
votingRole ("player1", "player1") = certainly ("player1","player2") 
votingRole ("player2", "player2") = certainly ("player2","player1")
votingRole (_,_)                 = uniform [("player1","player2"),("player2","player1")]


-- Voting on roles in trust game
votingRoleTrustSrc = Block [] []
                    [Line Nothing [] [] "dependentDecision \"player1\" (const [\"player1\", \"player2\"])" ["vote1"] ["0"],
                     Line Nothing [] [] "dependentDecision \"player2\" (const [\"player1\", \"player2\"])" ["vote2"] ["0"],
                     Line Nothing ["(vote1,vote2)"] [] "liftStochastic votingRole" ["roles"] [],
                     Line Nothing ["roles"] [] "trustGame" [] []]
                    [] []

votingRoleTrust = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2, roles) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "player1" (const ["player1", "player2"]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "player2" (const ["player1", "player2"]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), 0)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2) -> ((vote1, vote2), (vote1,vote2))) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((liftStochastic votingRole)))))) >>> (fromFunctions (\((vote1, vote2), roles) -> (vote1, vote2, roles)) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2, roles) -> ((vote1, vote2, roles), roles)) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((trustGame)))))) >>> (fromFunctions (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles)) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), ()))))))))) >>> (fromLens (\(vote1, vote2, roles) -> ()) (curry (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles)))))


--------------------------
-- 4. Voting role in ultimatum game

votingRoleUltimatumSrc = Block [] []
                    [Line Nothing [] [] "dependentDecision \"player1\" (const [\"player1\", \"player2\"])" ["vote1"] ["0"],
                     Line Nothing [] [] "dependentDecision \"player2\" (const [\"player1\", \"player2\"])" ["vote2"] ["0"],
                     Line Nothing ["(vote1,vote2)"] [] "liftStochastic votingRole" ["roles"] [],
                     Line Nothing ["roles"] [] "ultimatumGame" [] []]
                    [] []

votingRoleUltimatum = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(vote1, vote2, roles) -> ())) >>> (reindex (\(a1, a2, a3, a4) -> (((a1, a2), a3), a4)) ((((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\() -> ((), ())) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "player1" (const ["player1", "player2"]))))))) >>> (fromFunctions (\((), vote1) -> vote1) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), 0))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\vote1 -> (vote1, ())) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((dependentDecision "player2" (const ["player1", "player2"]))))))) >>> (fromFunctions (\(vote1, vote2) -> (vote1, vote2)) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), 0)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2) -> ((vote1, vote2), (vote1,vote2))) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((liftStochastic votingRole)))))) >>> (fromFunctions (\((vote1, vote2), roles) -> (vote1, vote2, roles)) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), ())))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(vote1, vote2, roles) -> ((vote1, vote2, roles), roles)) (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((ultimatumGame)))))) >>> (fromFunctions (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles)) (\(vote1, vote2, roles) -> ((vote1, vote2, roles), ()))))))))) >>> (fromLens (\(vote1, vote2, roles) -> ()) (curry (\((vote1, vote2, roles), ()) -> (vote1, vote2, roles)))))






-- 0 Stage games -----
    -- 0. Implement UG
    -- 1. Implement Trust Game

-- 1 Voting stage
    -- 0. Random choice of games
    -- 1. Refactor this stage to include voting (simple majority and quadractic)

-- 2 Refactor stage game
   -- 0. Add Voting into roles
   -- 1. Voting into concrete gambles

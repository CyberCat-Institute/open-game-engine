module Examples.EcologicalPublicGood.EcologicalPublicGood where

import Numeric.Probability.Distribution
import Preprocessor.AbstractSyntax
import Engine.BayesianOpenGames (L (..), (>>>>))
import Engine.BayesianDiagnostics

type D = T Double

data WorldState = Prosperous | Degraded deriving (Eq, Ord, Show)
data Action = Cooperate | Defect deriving (Eq, Ord, Show)

{- qc : leverage of collapse
   qr : leverage of recovery
   0 < both < 1, for now -}
transition :: (Double, Double, Action, Action, WorldState) -> D WorldState
transition (qc, qr, Cooperate, Cooperate, Prosperous) = certainly Prosperous
transition (qc, qr, Defect,    Defect,    Prosperous) = Cons [(Degraded, qc), (Prosperous, 1 - qc)]
transition (qc, qr, _,         _,         Prosperous) = Cons [(Degraded, qc/2), (Prosperous, 1 - qc/2)]
transition (qc, qr, Cooperate, Cooperate, Degraded)   = Cons [(Prosperous, qr), (Degraded, 1 - qr)]
transition (qc, qr, Defect,    Defect,    Degraded)   = certainly Degraded
transition (qc, qr, _,         _,         Degraded)   = Cons [(Prosperous, qr/2), (Degraded, 1 - qr/2)]

{- f : synergy factor
   c : contribution cost
   m : collapse impact -}
rewards :: Double -> Double -> Double -> WorldState -> Action -> Action -> WorldState -> (Double, Double)
rewards f c m Degraded _ _ _ = (m, m)
rewards f c m _ _ _ Degraded = (m, m)
rewards f c m Prosperous Cooperate Cooperate Prosperous = (f*c - c, f*c - c)
rewards f c m Prosperous Cooperate Defect    Prosperous = (f*c/2 - c, f*c/2)
rewards f c m Prosperous Defect    Cooperate Prosperous = (f*c/2, f*c/2 - c)
rewards f c m Prosperous Defect    Defect    Prosperous = (0, 0)

stageSource :: Block String String
stageSource = Block {
  blockCovariantInputs = ["currentState"], blockContravariantOutputs = ["fst (rewards f c m currentState action1 action2 nextState) + gamma*continuation1", "snd (rewards f c m currentState action1 action2 nextState) + gamma*continuation2"],
  blockLines = [
    Line {covariantInputs = ["currentState"], contravariantOutputs = [],
          matrix = "decision \"actor1\" [Cooperate, Defect]",
          covariantOutputs = ["action1"], contravariantInputs = ["fst (rewards f c m currentState action1 action2 nextState) + gamma*continuation1"]},
    Line {covariantInputs = ["currentState"], contravariantOutputs = [],
          matrix = "decision \"actor2\" [Cooperate, Defect]",
          covariantOutputs = ["action2"], contravariantInputs = ["snd (rewards f c m currentState action1 action2 nextState) + gamma*continuation2"]},
    Line {covariantInputs = ["qc", "qr", "action1", "action2", "currentState"], contravariantOutputs = [],
          matrix = "liftStochastic transition",
          covariantOutputs = ["nextState"], contravariantInputs = []}
  ],
  blockCovariantOutputs = ["nextState"], blockContravariantInputs = ["continuation1", "continuation2"]}

stage qc qr f c m gamma = reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\(currentState, action1, action2, nextState, continuation1, continuation2) -> (fst (rewards f c m currentState action1 action2 nextState) + gamma*continuation1, snd (rewards f c m currentState action1 action2 nextState) + gamma*continuation2))) >>> (reindex (\(a1, a2, a3) -> ((a1, a2), a3)) (((reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\currentState -> (currentState, currentState)) (\((currentState, action1, action2, nextState, continuation1, continuation2), ()) -> (currentState, action1, action2, nextState, continuation1, continuation2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "actor1" [Cooperate, Defect])))))) >>> (fromFunctions (\(currentState, action1) -> (currentState, action1)) (\(currentState, action1, action2, nextState, continuation1, continuation2) -> ((currentState, action1, action2, nextState, continuation1, continuation2), fst (rewards f c m currentState action1 action2 nextState) + gamma*continuation1))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(currentState, action1) -> ((currentState, action1), currentState)) (\((currentState, action1, action2, nextState, continuation1, continuation2), ()) -> (currentState, action1, action2, nextState, continuation1, continuation2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((decision "actor2" [Cooperate, Defect])))))) >>> (fromFunctions (\((currentState, action1), action2) -> (currentState, action1, action2)) (\(currentState, action1, action2, nextState, continuation1, continuation2) -> ((currentState, action1, action2, nextState, continuation1, continuation2), snd (rewards f c m currentState action1 action2 nextState) + gamma*continuation2)))))) >>> (reindex (\x -> (x, ())) ((reindex (\x -> ((), x)) ((fromFunctions (\(currentState, action1, action2) -> ((currentState, action1, action2), (qc, qr, action1, action2, currentState))) (\((currentState, action1, action2, nextState, continuation1, continuation2), ()) -> (currentState, action1, action2, nextState, continuation1, continuation2))) >>> (reindex (\x -> ((), x)) ((fromFunctions (\x -> x) (\x -> x)) &&& ((liftStochastic transition)))))) >>> (fromFunctions (\((currentState, action1, action2), nextState) -> (currentState, action1, action2, nextState)) (\(currentState, action1, action2, nextState, continuation1, continuation2) -> ((currentState, action1, action2, nextState, continuation1, continuation2), ()))))))))) >>> (fromLens (\(currentState, action1, action2, nextState) -> nextState) (curry (\((currentState, action1, action2, nextState), (continuation1, continuation2)) -> (currentState, action1, action2, nextState, continuation1, continuation2)))))

fromContinuation :: (x -> D y) -> L Double x y () ()
fromContinuation k = L (\x -> return (x, ())) (\x () -> k x)

toContinuation :: L Double x y () () -> x -> D y
toContinuation (L v u) x = do {(a, ()) <- v x; u a ()}

qc, qr, f, c, m, gamma :: Double
qc = 0.2
qr = 0.1
f = 1.2
c = 5.0
m = -4.0
gamma = 0.5

coopPolicy :: WorldState -> D Action
coopPolicy _ = certainly Cooperate

punishPolicy :: WorldState -> D Action
punishPolicy Prosperous = certainly Defect
punishPolicy Degraded = certainly Cooperate

valueIterator :: (WorldState -> D Action) -> (WorldState -> D Action) -> L Double WorldState (Double, Double) () () -> L Double WorldState (Double, Double) () ()
valueIterator a b k = play (stage qc qr f c m gamma) (a, b, ()) >>>> k

foo :: [L Double WorldState (Double, Double) () ()]
foo = iterate (valueIterator punishPolicy punishPolicy) (fromContinuation $ const $ certainly (-90, -90))

bar :: WorldState -> (Double, Double)
bar q = let xs = toContinuation (foo !! 15) q in (expected (fmap fst xs), expected (fmap snd xs))


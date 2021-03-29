module Engine.StatefulPayoffs where

-- Puting coplay into a state monad with payoff vectors as a state variable
-- This augments a game with an imperative program running backwards in time
-- An experimental pragmatic solution to a practical difficulty with agent identity

import           Engine.Diagnostics
import           Engine.OpenGamesClass

import           Control.Monad.State
import           Data.List                       (maximumBy)
import           Data.Ord                        (comparing)

-- This could trivially be generalised to any monad
data StatefulPayoffGame m v a x s y r = StatefulPayoffGame
  {play        :: a -> x -> y,
   coplay      :: a -> x -> r -> State v s,
   equilibrium :: x -> (y -> State v r) -> a -> m}

instance (Monoid m) => OG (StatefulPayoffGame m v) where
  fromLens v u = StatefulPayoffGame {
    play        = \() x -> v x,
    coplay      = \() x r -> return (u x r),
    equilibrium = \_ _ () -> mempty}
  reindex f g = StatefulPayoffGame {
    play        = \a -> play g (f a),
    coplay      = \a -> coplay g (f a),
    equilibrium = \h k a -> equilibrium g h k (f a)}
  (>>>) g1 g2 = StatefulPayoffGame {
    play        = \(a, b) -> play g2 b . play g1 a,
    coplay      = \(a, b) x q -> do {r <- coplay g2 b (play g1 a x) q; coplay g1 a x r},
    equilibrium = \h k (a, b) -> equilibrium g1 h (\y -> do {q <- k (play g2 b y); coplay g2 b y q}) a `mappend` equilibrium g2 (play g1 a h) k b}
  (&&&) g1 g2 = StatefulPayoffGame {
    play        = \(a, b) (x1, x2) -> (play g1 a x1, play g2 b x2),
    coplay      = \(a, b) (x1, x2) (r1, r2) -> do {s1 <- coplay g1 a x1 r1; s2 <- coplay g2 b x2 r2; return (s1, s2)},
    equilibrium = \(h1, h2) k (a, b) -> equilibrium g1 h1 (\y1 -> do {(r1, _) <- k (y1, play g2 b h2); return r1}) a
                              `mappend` equilibrium g2 h2 (\y2 -> do {(_, r2) <- k (play g1 a h1, y2); return r2}) b}
  (+++) g1 g2 = StatefulPayoffGame {
    play        = \(a, b) x -> case x of {Left x1 -> Left (play g1 a x1); Right x2 -> Right (play g2 b x2)},
    coplay      = \(a, b) x r -> case x of {Left x1 -> coplay g1 a x1 r; Right x2 -> coplay g2 b x2 r},
    equilibrium = \h k (a, b) -> case h of {Left h1 -> equilibrium g1 h1 (k . Left) a; Right h2 -> equilibrium g2 h2 (k . Right) b}}

-- Operate nonlocally on the payoff of a fixed agent
agentPayoff :: (Eq agent, Monoid m) => agent -> (p -> r -> p) -> StatefulPayoffGame m (agent -> p) () () () () r
agentPayoff agent f = StatefulPayoffGame {
  play        = \() () -> (),
  coplay      = \() () r -> do {v <- get; put (\agent' -> if agent == agent' then f (v agent) r else v agent')},
  equilibrium = \() _ () -> mempty}

-- Operate nonlocally on the payoff of an endogenously determined agent
rolePayoff :: (Eq agent, Monoid m) => (p -> r -> p) -> StatefulPayoffGame m (agent -> p) () agent () () r
rolePayoff f = StatefulPayoffGame {
  play        = \() _ -> (),
  coplay      = \() agent r -> do {v <- get; put (\agent' -> if agent == agent' then f (v agent) r else v agent')},
  equilibrium = \_ _ () -> mempty}

-- Decision by a fixed agent, adding payoff numerically to state
agentDecision :: (Eq agent, Show agent, Show x, Show y, Num r, Ord r, Show r)
              => agent -> [y] -> StatefulPayoffGame [DiagnosticInfo] (agent -> r) (x -> y) x () y r
agentDecision agent ys = StatefulPayoffGame {
  play = id,
  coplay = \_ _ r -> do {v <- get; put (\agent' -> if agent == agent' then v agent + r else v agent')},
  equilibrium = \h k a -> let u y = let (r, v) = runState (k y) (const 0) in v agent + r
                              (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
                           in if u (a h) >= optimalPayoff then []
                                                          else [DiagnosticInfo {player = show agent,
                                                                                Engine.Diagnostics.state = show h,
                                                                                unobservableState = "",
                                                                                strategy = show (a h),
                                                                                Engine.Diagnostics.payoff = show (u (a h)),
                                                                                optimalMove = show (optimalPlay),
                                                                                optimalPayoff = show (optimalPayoff)}]}

-- Decision by an endogenously determined agent, adding payoff numerically to state
roleDecision :: (Eq agent, Show agent, Show x, Show y, Num r, Ord r, Show r)
             => [y] -> StatefulPayoffGame [DiagnosticInfo] (agent -> r) (agent -> x -> y) (agent, x) () y r
roleDecision ys = StatefulPayoffGame {
  play        = uncurry,
  coplay      = \_ (agent, _) r -> do {v <- get; put (\agent' -> if agent == agent' then v agent + r else v agent')},
  equilibrium = \(agent, h) k a -> let u y = let (r, v) = runState (k y) (const 0) in v agent + r
                                       (optimalPlay, optimalPayoff) = maximumBy (comparing snd) [(y, u y) | y <- ys]
                                    in if u (a agent h) >= optimalPayoff then []
                                                                         else [DiagnosticInfo {player = show agent,
                                                                                               Engine.Diagnostics.state = show h,
                                                                                               unobservableState = "",
                                                                                               strategy = show (a agent h),
                                                                                               Engine.Diagnostics.payoff = show (u (a agent h)),
                                                                                               optimalMove = show (optimalPlay),
                                                                                               optimalPayoff = show (optimalPayoff)}]}

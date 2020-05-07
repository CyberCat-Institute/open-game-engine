module OpenGames.Engine.PureOpenGames where

-- Open games with pure strategies
-- Generic over a monoid of truth-values, which can be used to return debugging information about a strategy profile

import OpenGames.Engine.OpenGamesClass

data PureOpenGame m a x s y r = PureOpenGame
  {play        :: a -> x -> y,
   coplay      :: a -> x -> r -> s,
   equilibrium :: x -> (y -> r) -> a -> m}

instance (Monoid m) => OG (PureOpenGame m) where
  fromLens v u = PureOpenGame {
    play        = const v,
    coplay      = const u,
    equilibrium = \_ _ () -> mempty}
  reindex f g = PureOpenGame {
    play        = \a -> play g (f a),
    coplay      = \a -> coplay g (f a),
    equilibrium = \h k a -> equilibrium g h k (f a)}
  (>>>) g1 g2 = PureOpenGame {
    play        = \(a, b) -> play g2 b . play g1 a,
    coplay      = \(a, b) x q -> coplay g1 a x (coplay g2 b (play g1 a x) q),
    equilibrium = \h k (a, b) -> equilibrium g1 h (\y -> coplay g2 b y (k (play g2 b y))) a `mappend` equilibrium g2 (play g1 a h) k b}
  (&&&) g1 g2 = PureOpenGame {
    play        = \(a, b) (x1, x2) -> (play g1 a x1, play g2 b x2),
    coplay      = \(a, b) (x1, x2) (r1, r2) -> (coplay g1 a x1 r1, coplay g2 b x2 r2),
    equilibrium = \(h1, h2) k (a, b) -> equilibrium g1 h1 (\y1 -> fst (k (y1, play g2 b h2))) a `mappend` equilibrium g2 h2 (\y2 -> snd (k (play g1 a h1, y2))) b}
  (+++) g1 g2 = PureOpenGame {
    play        = \(a, b) x -> case x of {Left x1 -> Left (play g1 a x1); Right x2 -> Right (play g2 b x2)},
    coplay      = \(a, b) x r -> case x of {Left x1 -> coplay g1 a x1 r; Right x2 -> coplay g2 b x2 r},
    equilibrium = \h k (a, b) -> case h of {Left h1 -> equilibrium g1 h1 (k . Left) a; Right h2 -> equilibrium g2 h2 (k . Right) b}}

pureDecision :: (Ord r) => [y] -> PureOpenGame Bool (x -> y) x () y r
pureDecision ys = PureOpenGame
  {play = \a -> a,
   coplay = \_ _ _ -> (),
   equilibrium = \h k a -> all (\y -> k (a h) >= k y) ys}

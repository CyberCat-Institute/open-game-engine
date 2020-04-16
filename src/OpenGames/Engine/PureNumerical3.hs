{-# LANGUAGE RankNTypes #-}

module OpenGames.Engine.PureNumerical3 where

data SmoothContext p x y = SmoothContext (p, x) ((p, y) -> y)

data SmoothOG a x y = SmoothOG {
  play :: a -> x -> (y, y -> x),
  eq :: forall p. SmoothContext p x y -> [a -> Double]}

decision0 :: SmoothOG Double () Double
decision0 = SmoothOG {
  play = \x () -> (x, \dx -> ()),
  eq = \(SmoothContext (p, ()) k) -> [\x -> k (p, x)]}
{-
decision1 :: (Monoid x) => SmoothOG Double x (x, Double)
decision1 = SmoothOG {
  play = \y x -> ((x, y), \dy -> mempty),
  eq = \(SmoothContext (p, x) k) -> [\y -> _]}
  -- \(SmoothContext (p1, x) k) -> [\(p2, y) -> snd (k (p2, (x, y)))]}

-}
module OpenGames.Engine.Costrategies where

import Data.Profunctor

type Lens s t a b = s -> (a, b -> t)

data CostrategyGame a b x s y r = CostrategyGame {
  play :: Lens (a, x) (b, s) y r,
  evaluate :: (a -> b) -> a -> Bool
}

reindex :: Lens a b a' b' -> CostrategyGame a' b' x s y r -> CostrategyGame a b x s y r
reindex = \l g -> CostrategyGame {
  play = \(a, x) -> let (a', u) = l a
                        (y, k) = play g (a', x)
                     in (y, \r -> let (b', s) = k r
                                   in (u b', s)),
  evaluate = \k a -> let (a', u) = l a
                      in evaluate g undefined a'
}

data PositiveGame a b x s y r = PositiveGame {
  play' :: a -> Lens x s y r,
  evaluate' :: x -> (y -> r) -> a -> b
}

reindex' :: Lens a b a' b' -> PositiveGame a' b' x s y r -> PositiveGame a b x s y r
reindex' l g = PositiveGame {
  play' = \a -> let (a', _) = l a in play' g a',
  evaluate' = \x k a -> let (a', u) = l a
                         in u (evaluate' g x k a')
}

sequential' :: PositiveGame a b x s y r -> PositiveGame a' b' y r z q -> PositiveGame (a, a') (b, b') x s z q
sequential' g h = PositiveGame {
  play' = \(a, a') x -> let (y, u) = play' g a x
                            (z, u') = play' h a' y
                         in (z, u . u'),
  evaluate' = \x k (a, a') -> let (y, _) = play' g a x
                                  k' y = let (z, u') = play' h a' y
                                          in u' (k z)
                               in (evaluate' g x k' a, evaluate' h y k a')
}

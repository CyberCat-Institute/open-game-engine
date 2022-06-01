{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenGames.Engine.OpenGames
 ( OpenGame(..)
 , lift
 , reindex
 , (>>>)
 , (&&&)
 ) where


import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL

data OpenGame o c a b x s y r = OpenGame {
  play :: List a -> o x s y r,
  evaluate :: List a -> c x s y r -> List b
}

lift :: o x s y r -> OpenGame o c '[] '[] x s y r
lift o = OpenGame {
  play = \Nil -> o,
  evaluate = \Nil _ -> Nil
}

reindex :: (List a -> List a') -> (List a -> List b' -> List b)
        -> OpenGame o c a' b' x s y r -> OpenGame o c a b x s y r
reindex v u g = OpenGame {
  play = \a -> play g (v a),
  evaluate = \a c -> u a (evaluate g (v a) c)
}

(>>>) :: (Optic o, Context c o, Unappend a, Unappend b)
      => OpenGame o c a b x s y r -> OpenGame o c a' b' y r z q
      -> OpenGame o c (a +:+ a') (b +:+ b') x s z q
(>>>) g h = OpenGame {
  play = \as -> case unappend as of (a, a') -> play g a >>>> play h a',
  evaluate = \as c -> case unappend as of (a, a') -> evaluate g a (cmap identity (play h a') c)
                                                  +:+ evaluate h a' (cmap (play g a) identity c)
}

(&&&) :: (Optic o, Context c o, Unappend a, Unappend b, Show x, Show x')
      => OpenGame o c a b x s y r -> OpenGame o c a' b' x' s' y' r'
      -> OpenGame o c (a +:+ a') (b +:+ b') (x, x') (s, s') (y, y') (r, r')
(&&&) g h = OpenGame {
  play = \as -> case unappend as of (a, a') -> play g a &&&& play h a',
  evaluate = \as c -> case unappend as of (a, a') -> evaluate g a (play h a' \\ c)
                                                  +:+ evaluate h a' (play g a // c)
}

interleave2 :: forall a a' b b' x y s r. (Unappend a, Unappend b)
            => OpenGame StochasticStatefulOptic StochasticStatefulContext a b (x, Maybe y) s y r
            -> OpenGame StochasticStatefulOptic StochasticStatefulContext a' b' (x, Maybe y) s y r
            -> OpenGame StochasticStatefulOptic StochasticStatefulContext (a +:+ a') (b +:+ b') (x, Bool) (s, s) (y, y) (r, r)
interleave2 g g' = OpenGame {
  play = \as -> case unappend as of
                     (a :: List a, a' :: List a') ->
                       case (play g a, play g' a') of
                         (StochasticStatefulOptic v1 u1, StochasticStatefulOptic v2 u2) ->
                           StochasticStatefulOptic (\(x, b) -> if b then do (z, y) <- v1 (x, Nothing)
                                                                            (z', y') <- v2 (x, Just y)
                                                                            return ((z, z', True), (y, y'))
                                                                    else do (z', y') <- v2 (x, Nothing)
                                                                            (z, y) <- v1 (x, Just y')
                                                                            return ((z, z', False), (y, y')))
                                                   (\(z, z', b) (r, r') -> if b then do s2 <- u2 z' r'
                                                                                        s1 <- u1 z r
                                                                                        return (s1, s2)
                                                                                else do s1 <- u1 z r
                                                                                        s2 <- u2 z' r'
                                                                                        return (s1, s2)),
  evaluate = \as c -> case unappend as of
                           (a :: List a, a' :: List a') ->
                             case (play g a, play g' a', c) of
                               (StochasticStatefulOptic v1 u1, StochasticStatefulOptic v2 u2, StochasticStatefulContext h k) ->
                                     evaluate g a (StochasticStatefulContext (do )
                                                                             undefined)
                                 +:+ evaluate g' a' undefined
}

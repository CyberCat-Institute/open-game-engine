{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module OpenGames.Engine.OpenGames where
--  ( OpenGame(..)
--  , lift
--  , reindex
--  , (>>>)
--  , (&&&)
--  ) where


import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL
import OpenGames.Engine.Nat
import OpenGames.Engine.Vec

data OpenGame o c (n :: Nat) a b x s y r = OpenGame {
  play :: a -> o x s y r,
  evaluate :: a -> c x s y r -> b
}

lift :: o x s y r -> OpenGame o c 'Z () () x s y r
lift o = OpenGame {
  play = \() -> o,
  evaluate = \() _ -> ()
}

-- reindex :: (List a -> List a') -> (List a -> List b' -> List b)
--         -> OpenGame o c n a' b' x s y r -> OpenGame o c n a b x s y r
-- reindex v u g = OpenGame {
--   play = \a -> play g (v a),
--   evaluate = \a c -> u a (evaluate g (v a) c)
-- }

flatten :: (a, b) -> Flatten a b
flatten = undefined

unflatten :: Flatten a b -> (a, b)
unflatten = undefined

(>>>) :: (Optic o, Context c o)
      => OpenGame o c n1 a b x s y r -> OpenGame o c n2 a' b' y r z q
      -> OpenGame o c (Add n1 n2) (Flatten a a') (Flatten b b') x s z q
(>>>) g h = OpenGame {
  play = \as -> case unflatten as of (a, a') -> play g a >>>> play h a',
  evaluate = \as c -> case unflatten as of
    (a, a') -> flatten  ( evaluate g a (cmap identity (play h a') c)
                        , evaluate h a' (cmap (play g a) identity c))
}

(&&&) :: (Optic o, Context c o, Show x, Show x')
      => OpenGame o c n1 a b x s y r -> OpenGame o c n2 a' b' x' s' y' r'
      -> OpenGame o c (Add n1 n2) (Flatten a a') (Flatten b b') (x, x') (s, s') (y, y') (r, r')
(&&&) g h = OpenGame {
  play = \as -> case unflatten as of (a, a') -> play g a &&&& play h a',
  evaluate = \as c -> case unflatten as of
    (a, a') -> flatten  ( evaluate g a (play h a' \\ c)
                        , evaluate h a' (play g a // c))
}

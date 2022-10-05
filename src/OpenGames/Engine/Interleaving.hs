{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module OpenGames.Engine.Interleaving where

-- import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.TLL
import OpenGames.Engine.OpenGames
import OpenGames.Engine.OpticClass

foldseq :: [StochasticStatefulOptic (s, [a]) t a b] -> StochasticStatefulOptic [s] [t] [a] [b]
foldseq = foldr foldseqcons foldseqnil

foldseqcons :: StochasticStatefulOptic (s, [a]) t a b
            -> StochasticStatefulOptic [s] [t] [a] [b]
            -> StochasticStatefulOptic [s] [t] [a] [b]
foldseqcons (StochasticStatefulOptic v1 u1) (StochasticStatefulOptic v2 u2)
  = StochasticStatefulOptic (\(s:ss) -> do { (z2, as) <- v2 ss
                                           ; (z1, a) <- v1 (s, as)
                                           ; return ((z1, z2), a : as) })
                            (\(z1, z2) (b:bs) -> do { t <- u1 z1 b ; ts <- u2 z2 bs ; return (t:ts) })

foldseqnil :: StochasticStatefulOptic [s] [t] [a] [b]
foldseqnil = lens (\[] -> []) (\[] [] -> [])

test1 :: StochasticStatefulOptic (Int, [String]) () String ()
test1 = StochasticStatefulOptic (\(n, ss) -> return ((), (if null ss then "" else head ss) ++ show n)) (\() () -> return ())
test2 :: StochasticStatefulOptic (Int, [String]) () String ()
test2 = StochasticStatefulOptic (\(n, ss) -> return ((),show (length ss))) (\() () -> return ())

{-}
interleave2 :: (Optic o, Context c o, Unappend a, Unappend a')
           => OpenGame o c a b (x, Maybe y') s y r
           -> OpenGame o c a' b' (x', Maybe y) s' y' r'
           -> OpenGame o c  (a +:+ a') (b +:+ b') (x, x') (s, s') (y, y') (r, r')
interleave2 g h = [opengame|
  inputs: x, x' ;
  feedback: s, s' ;

  :---:

  inputs: x, Nothing ;
  feedback : s ;
  operation: g ;
  outputs : y ;
  returns : r ;

  inputs : x', Just y ;
  feedback : s' ;
  operation : h ;
  outputs : y' ;
  returns : r' ;

  :---:

  outputs : y, y' ;
  returns : r, r' ;
|]
-}

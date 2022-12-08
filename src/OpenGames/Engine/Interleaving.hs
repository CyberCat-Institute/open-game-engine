{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module OpenGames.Engine.Interleaving where

-- import OpenGames
import OpenGames.Preprocessor
import OpenGames.Engine.TLL
import OpenGames.Engine.Nat
import OpenGames.Engine.Vec (Vec(..))
import OpenGames.Engine.OpenGames
import OpenGames.Engine.OpticClass
import OpenGames.Engine.BayesianGames hiding (fromFunctions)

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
--
-- > case (foldseq $ replicate 5 test1) of StochasticStatefulOptic v u -> fmap snd (v $ reverse [1,2,3,4,5])
-- fromFreqs [(["12345","1234","123","12","1"],1.0)]
-- > fmap reverse it
-- fromFreqs [(["1","12","123","1234","12345"],1.0)]

permuteList :: [(Int, a)] -> [a]
permuteList as = map ((map snd as) !!) (map fst as)

permuteListLens :: StochasticStatefulOptic [(Int, s)] [t] [s] [t]
permuteListLens = lens permuteList (\s -> permuteList . zip (map fst s))

controlledInterleave :: Vec n (StochasticStatefulBayesianOpenGame a b (x, [y]) s y r) -> Natural n
                     -> StochasticStatefulBayesianOpenGame (CatRepeat n a) (CatRepeat n '[[Int] -> List b]) [(Int, x)] [s] [y] [r]
controlledInterleave Empty Zero = OpenGame {
  play = \Nil -> foldseqnil,
  evaluate = \Nil _ -> Nil
}
controlledInterleave (v :> vs) (Succ n) =
  undefined

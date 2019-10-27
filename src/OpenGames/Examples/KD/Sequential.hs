module OpenGames.Examples.KD.Sequential where

import OpenGames.Engine.BayesianDiagnosticsTLL
import OpenGames.Examples.Sequential

-- This doesn't typecheck and I don't know why
-- It was generated from https://kdmoncatog.glitch.me/#pixels=Px-U%0APxQU%0AP--U&context=P%20:%20-%3E%20X%20r%0AQ%20:%20X%20-%3E%20Y%20r%0AU%20:%20X%20Y%20r%20r%20-%3E%0Ax%20:%20X%20-%3E%20X%20X%20

{-
sequentialKD = let p = decision "player1" [GoLeft, GoRight]
                   q = decision "player2" [GoLeft, GoRight]
                   x = fromFunctions (\x -> (x, x)) id
                   u = counitFunction (\(x, y) -> (sequentialMatrix2 x y, sequentialMatrix1 x y)) 
  in (p >>> (fromFunctions (\(x0_0, a0_1) -> (x0_0, a0_1)) (\((), ()) -> ()) >>> (x &&& (fromFunctions (\a0 -> a0) (\() -> ()))) >>> fromFunctions (\((x0_0, x1_0), a0_1) -> (x0_0, x1_0, a0_1)) (\() -> ((), ()))) >>> (fromFunctions (\(a0_0, x0_1, a0_2) -> ((a0_0, x0_1), a0_2)) (\(((), ()), ()) -> ()) >>> (((fromFunctions (\a0 -> a0) (\() -> ())) &&& q) &&& (fromFunctions (\a0 -> a0) (\() -> ()))) >>> fromFunctions (\((a0_0, y0_1), a0_2) -> (a0_0, y0_1, a0_2)) (\r1_1 -> (((), r1_1), ()))) >>> u)
-}


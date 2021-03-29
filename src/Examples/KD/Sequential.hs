{-# LANGUAGE DataKinds #-}
module Examples.KD.Sequential where

import Engine.BayesianDiagnosticsTLL
import Examples.Sequential
import Engine.BayesianOpenGames

-- https://kdmoncatog.glitch.me/#pixels=PxXU%0APxQU%0APrrU&context=P%20:%20-%3E%20X%20r%0AQ%20:%20X%20-%3E%20Y%20r%0AU%20:%20X%20Y%20r%20r%20-%3E%0Ax%20:%20X%20-%3E%20X%20X%20

sequentialKD :: BayesianDiagnosticOpenGameTLL 
  '[ () -> D SequentialMove, SequentialMove -> D SequentialMove ] () () () ()
sequentialKD = let p = decision "player1" [GoLeft, GoRight]
                   q = decision "player2" [GoLeft, GoRight]
                   x = fromFunctions (\x -> (x, x)) id
                   u = counitFunction (\(x, y) -> (sequentialMatrix2 x y, sequentialMatrix1 x y)) 
  in (p >>> (fromFunctions (\x0_0 -> (x0_0, ())) (\((), r_1) -> r_1) >>> ((x >>> (fromFunctions (\(x_0, x0_1) -> (x_0, x0_1)) (\((), ()) -> ()) >>> (fromFunctions id id &&& q) >>> fromFunctions (\(x_0, y0_1) -> (x_0, y0_1)) (\r1_1 -> ((), r1_1)))) &&& fromFunctions id id) >>> fromFunctions (\((x_0_0, y0_1_0), ()) -> (x_0_0, y0_1_0)) (\(r1_1_0, r_1) -> (r1_1_0, r_1))) >>> u)

module Examples.KD.Bimatrix where

-- Bimatrix with code generated from the KD editor

import Engine.BayesianDiagnosticsTLL
import Examples.Bimatrix

matchingPenniesKD = let a = decision "player1" [Heads, Tails]
                        b = a
                        u = counitFunction $ \(x, y) -> (matchingPenniesMatrix2 x y, matchingPenniesMatrix1 x y)
  in ((fromFunctions (\() -> ((), ())) (\((), ()) -> ()) >>> (a &&& b) >>> fromFunctions (\(x0_0, y0_1) -> (x0_0, y0_1)) (\(r1_0, r1_1) -> (r1_0, r1_1))) >>> u)


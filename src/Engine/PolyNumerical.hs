module Engine.PolyNumerical where

import Data.Poly

-- Replacing functions with polynomials

-- List of coefficients of a 1-variable polynomial
newtype Poly = Poly [Double]

newtype PolyLens = PolyLens [(Double, [Double])]

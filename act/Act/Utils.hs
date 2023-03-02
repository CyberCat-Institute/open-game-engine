module Act.Utils where

import Data.Char (toUpper)

capitalise :: String -> String
capitalise [] = []
capitalise (x : xs) = toUpper x : xs


module Main where

import Syntax.Annotated
import CLI
import Lex
import System.Environment

main :: IO ()
main = do
  --[_, f] <- getArgs
  contents <- readFile "simple.act"
  let v = lexer contents
  print v
  -- validation (prettyErrs contents) print (parse $ lexer contents)
  -- pure ()

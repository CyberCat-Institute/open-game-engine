{-# LANGUAGE DeriveFunctor #-}
module Preprocessor.Parser where

import Text.Parsec
import Text.Parsec.String
import Language.Haskell.TH
import Preprocessor.Lambda
import Preprocessor.AbstractSyntax

word :: Parser String
word = many1 alphaNum

quoted :: Parser String
quoted = char '"' *> manyTill anyChar (char '"')

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (x, y, z, w, v) = f x y z w v

lineSep :: String -> Parser ()
lineSep str = spaces <* string str <* spaces

realParser :: Parser (Block Pattern Lambda)
realParser =  parseBlock parsePattern expr <* eof

parseLambda :: String -> Either ParseError (Block Pattern Lambda)
parseLambda = parse realParser "realParser"

parseVerbose :: String -> Either ParseError (Block Pattern Lambda)
parseVerbose = parse (parseVerboseSyntax parsePattern expr) "verbose parser"



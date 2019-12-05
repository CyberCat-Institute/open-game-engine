{-# LANGUAGE RecordWildCards #-}

module OpenGames.Preprocessor.Parser
  ( runLineParser
  ) where

import OpenGames.Preprocessor.AbstractSyntax
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char

type Parser = Parsec () String
type ParserError = ParseErrorBundle String ()

-- | Run the Line parser on a string.
runLineParser :: String -> Either ParserError Line
runLineParser  = runParser lineParser ""

-- | Parses a Line. The syntax is
-- @cvo, ..., cvo' | cno, ..., cno' <- matrix -< cvi, ..., cvi' | cni, ..., cvi'@
--
lineParser :: Parser Line
lineParser = do
    (covariantOutputs, contravariantOutputs) <- lineOutputsParser
    matrix <- lineMatrixParser
    (covariantInputs, contravariantInputs) <- lineInputsParser
    pure Line {..}

-- | Parse the @cvo, ..., cvo' | cno, ..., cno'@ part of a line.
lineOutputsParser :: Parser ([String], [String])
lineOutputsParser = cvcnParser nameParser nameParser

-- | Parse the @cvi, ..., cvi' | cni, ..., cni'@ part of a line.
lineInputsParser :: Parser ([String], [String])
lineInputsParser = cvcnParser exprParser exprParser

-- | Parse a @covariant list | contravariant list@ pair, given a
-- parser each element of each list.
cvcnParser :: Parser a -> Parser b -> Parser ([a], [b])
cvcnParser p1 p2 = do
  v1 <- commaSeparatedList p1
  tok "|"
  v2 <- commaSeparatedList p2
  pure (v1, v2)

-- | Parse a comma-separated list.
commaSeparatedList :: Parser a -> Parser [a]
commaSeparatedList p = ((:) <$> p <*> many (tok "," >> p)) <|> pure []

-- | Parse a token and the whitespace space after it (but ignore the result).
tok :: String -> Parser ()
tok t = string t >> space >> pure ()

-- | Parse the @<- matrix -<@
lineMatrixParser :: Parser String
lineMatrixParser = do
  tok "<-"
  matrix <- nameParser
  tok "-<"
  pure matrix

-- | Parse a single name.
nameParser :: Parser String
nameParser = do
  space
  h <- (char '_' <|> letterChar)
  t <- many (char '_' <|> alphaNumChar)
  pure (h:t)

-- | Parse an expression. I'm going to assume, for now,
-- that an expression is either a name, or it's wrapped
-- in parentheses, in which case it could be a sequence
-- of expression tokens and/or additional nested
-- expressions.
exprParser :: Parser String
exprParser = nameParser <|> do
  tok "("
  x <- many exprTokenParser
  tok ")"
  pure ("(" ++ unwords x ++ ")")

exprTokenParser :: Parser String
exprTokenParser =
  exprParser <|>
  takeWhile1P Nothing (\c ->
    not (isSpace c)
    && not (c == '(')
    && not (c == ')'))


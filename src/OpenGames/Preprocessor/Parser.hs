{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- A parser for Lambda Calculus
module OpenGames.Preprocessor.Parser
  ( Lambda(..)
  , Literal(..)
  , LStmt(..)
  , LRange(..)
  , Pattern(..)
  , expr
  , parseLine
  , parseBlock
  , parsePattern
  , parseVerboseSyntax
  , parseVerboseLine
  , parseLambda
  , parseVerbose
  ) where

import           Data.Char
import           Text.Parsec hiding (Line)
import           Text.Parsec.Prim
import           Text.Parsec.Error
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Token    as Tok
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import OpenGames.Preprocessor.BlockSyntax

type Name = String

data LRange =
              LFromR Lambda
            | LFromThenR Lambda Lambda
            | LFromToR Lambda Lambda
            | LFromThenToR Lambda Lambda Lambda
            deriving (Eq, Show)

data Lambda
  = Var Name
  | App Lambda Lambda
  | Lam Pattern Lambda
  | Lit Literal
  | LList [Lambda]
  | Do [(Maybe Name, Lambda)]
  | Tuple Lambda Lambda [Lambda]
  | Range LRange
  | IfThenElse Lambda Lambda Lambda
  | Ifix String Lambda Lambda
  | PFix String Lambda
  | LLet Pattern Lambda Lambda
  | LComp [LStmt]
  deriving (Eq, Show)

data LStmt
  = LBindS Pattern Lambda
  | LNoBindS Lambda
  | LLetS Pattern Lambda
  deriving (Eq, Show)


data Literal
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Eq, Show)

data Pattern
  = PVar Name           -- Just a variable
  | PTuple [Pattern]    -- Tuple pattern
  | PCon Name [Pattern] -- constructor pattern
  | PList [Pattern]     -- List pattern
  | PLit Literal        -- Match a literal exactly
  deriving (Eq, Show)

langaugeKeywords = ["if", "then", "else", "data", "import", "do", "let", "in" -- language keywords
                   , "inputs", "outputs" -- the covariant arguments
                   , "feedback", "returns" -- the contravariant arguments
                   , "operation" -- the operation of the line
                   , "label" -- the label of a line
                   ]

modifiedHaskell :: LanguageDef st
modifiedHaskell = emptyDef
                { Tok.commentStart   = "{-"
                , Tok.commentEnd     = "-}"
                , Tok.commentLine    = "//"
                , Tok.nestedComments = True
                , Tok.identStart     = letter
                , Tok.identLetter    = alphaNum <|> oneOf "_'"
                , Tok.opStart        = Tok.opLetter modifiedHaskell
                , Tok.opLetter       = oneOf ":!#$%&*+./<=>?@\\^-~"
                , Tok.reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>", "$", "-<", ";", "|", "<<=", "||", "=>>"]
                , Tok.reservedNames  = langaugeKeywords
                , Tok.caseSensitive  = True
                }


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser modifiedHaskell

colon = Tok.colon lexer

semi = Tok.semi lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

comma :: Parser String
comma = Tok.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

mkInfix :: String -> Parser (Lambda -> Lambda -> Lambda)
mkInfix op = reservedOp op >> pure (Ifix op)

operators = [ [Infix (mkInfix "$") AssocRight]
            , [Infix (mkInfix ">>=") AssocLeft]
            , [Infix (mkInfix "*>") AssocLeft
              ,Infix (mkInfix "<*") AssocLeft
              ,Infix (mkInfix "<*>") AssocLeft
              ,Infix (mkInfix ">") AssocLeft
              ,Infix (mkInfix "<") AssocLeft
              ,Infix (mkInfix "<=") AssocNone
              ,Infix (mkInfix ">=") AssocNone
              ]
            , [Prefix (reservedOp "-" *> pure (PFix "-"))]
            , [Infix (mkInfix "++") AssocRight]
            , [Infix (mkInfix "+") AssocLeft
              ,Infix (mkInfix "-") AssocLeft
              ,Infix (mkInfix "<>") AssocRight
              ]
            , [Infix (mkInfix "*") AssocLeft
              ,Infix (mkInfix "/") AssocLeft]
            ]

infixParser = buildExpressionParser operators

-- ^ Parse an Integer using the tokenizer
natural :: Parser Integer
natural = Tok.natural lexer

-- ^ Parse a variable as a Lambda term
variable :: Parser Lambda
variable = identifier >>= (return . Var)

-- ^ Parse an Integer as a Lambda term
number :: Parser Lambda
number = natural >>= return . (\x -> (Lit (LInt (fromIntegral x))))

-- ^ Parse a string literal as a Lambda term
strLit :: Parser Lambda
strLit = Lit . LString <$> Tok.stringLiteral lexer

-- ^ Parse two things in sequence and bundle them in a pair
pair :: Parser a -> Parser b -> Parser (a, b)
pair p1 p2 = do r1 <- p1; r2 <- p2; pure (r1, r2)

parseLit :: Parser Literal
parseLit = LString <$> Tok.stringLiteral lexer
       <|> LInt . fromIntegral <$> natural

parsePattern :: Parser Pattern
parsePattern =
  (do p <- identifier ;
      if isUpper (head p) then PCon p <$> many parsePattern
                          else pure $ PVar p)
  <|> PTuple <$> parens (commaSep parsePattern)
  <|> PList <$> brackets (commaSep parsePattern)
  <|> PLit <$> parseLit

doNotation :: Parser Lambda
doNotation =
  Do <$> (reserved "do"
      *> braces (statement `sepEndBy` reservedOp ";"))
  where
    statement :: Parser (Maybe String, Lambda)
    statement = try ((Just <$> identifier <* reservedOp "<-") `pair` expr)
            <|> ((Nothing :: Maybe String,) <$> expr)

parseLet :: Parser Lambda
parseLet = do
  reserved "let"
  varName <- parsePattern
  reservedOp "="
  value <- expr
  reserved "in"
  body <- expr
  pure (LLet varName value body)

parseTuple :: Parser Lambda
parseTuple = do
  f <- expr
  Tok.comma lexer
  s <- expr
  rest <- many (Tok.comma lexer *> expr)
  pure (Tuple f s rest)

lambda :: Parser Lambda
lambda = do
  reservedOp "\\"
  args <- many1 parsePattern
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

ifExp :: Parser Lambda
ifExp = do
  reserved "if"
  prd <- term
  reserved "then"
  thn <- term
  reserved "else"
  els <- term
  return $ IfThenElse prd thn els

-- ^ Parse a bracketed expression
-- ^ Those are a bit complicated because they can be either a list
-- ^ Or a range with begining and end
-- ^ Or an infinite list
-- ^ Or a range with begining and end and a step size
-- ^ Or an infinite list with a step size
bracketed :: Parser Lambda
bracketed =
  (do { e1 <- expr -- first check we have at least one element
      ; do { e2 <- comma *> expr -- then expect a comma
           -- after the comma we either have more elements or …
           ; (comma *> (LList . (\x -> e1 : e2 : x) <$> commaSep expr)
           -- … or we have a range starting with `..`
             <|> (Range <$> (reservedOp ".." *>
                              (LFromThenToR e1 e2 <$> expr
                               <|> pure (LFromR e1)))))
             <|> pure (LList [e1, e2])
           } -- if there is no comma, check if there is `..` to parse a range
             <|> reservedOp ".." *> (Range <$> (LFromToR e1 <$> expr <|> pure (LFromR e1)))
      })
  <|> pure (LList [])

comprehension :: Parser Lambda
comprehension = do
  e <- expr
  reservedOp "|"
  LComp <$> ((++ [LNoBindS e]) <$> (sepBy1 parseStmt comma))
    where
      parseStmt :: Parser LStmt
      parseStmt = LBindS <$> parsePattern <*> (reservedOp "<-" *> expr)
              <|> LLetS <$> (reserved "let" *> parsePattern) <*> expr
              <|> LNoBindS <$> expr

term :: Parser Lambda
term =  parens (try parseTuple <|> expr)
    <|> ifExp
    <|> lambda
    <|> variable
    <|> number
    <|> strLit
    <|> brackets (try comprehension <|> bracketed)
    <|> doNotation
    <|> parseLet

appl :: Parser Lambda
appl = do
  es <- many1 term
  return (foldl1 App es)

expr :: Parser Lambda
expr =  infixParser appl


parseLine :: Parser p -> Parser e -> Parser (Line (Maybe String) p e)
parseLine parseP parseE = do
    covo <- (commaSep parseP <* reservedOp "|")
    coni <- (commaSep parseE  <* reservedOp "<-")
    matrix <- (parseE <* reservedOp "-<")
    cono <- (commaSep parseP <* reservedOp "|")
    covi <- (commaSep parseE <* reservedOp ";")
    pure (Line Nothing covi cono matrix covo coni)

parseBlock :: Parser p -> Parser e -> Parser (Block p e)
parseBlock parseP parseE = do
    covi <- (commaSep parseP <* reservedOp "||")
    cono <- (commaSep parseE <* reservedOp "=>>")
    lines <- (many (parseLine parseP parseE) <* reservedOp "<<=")
    coni <- (commaSep parseP <* reservedOp "||")
    covo <- (commaSep parseE)
    pure (Block covi cono lines covo coni)

parseTwoLines :: String -> String -> Parser p -> Parser e -> Parser ([p], [e])
parseTwoLines kw1 kw2 parseP parseE =
    pair (reserved kw1 *> colon *> commaSep parseP <* semi )
         (option [] (reserved kw2 *> colon *> commaSep parseE <* semi ))
 <|> (([], ) <$> (reserved kw2 *> colon *> commaSep parseE <* semi))

parseInput = parseTwoLines "inputs" "feedback"

parseOutput = parseTwoLines "outputs" "returns"

parseDelimiter = colon *> many1 (string "-") <* colon

parseVerboseLine :: Parser p -> Parser e -> Parser (Line (Maybe String) p e)
parseVerboseLine parseP parseE = do
  lbl <- optionMaybe (reserved "label" *> colon *> manyTill anyChar semi)
  (input, feedback) <- option ([], []) (parseInput parseE parseP)
  program <- reserved "operation" *> colon *> parseE <* semi
  (outputs,returns) <- option ([], []) (parseOutput parseP parseE)
  pure $ Line lbl input feedback program outputs returns

parseVerboseSyntax :: Parser p -> Parser e -> Parser (Block p e)
parseVerboseSyntax parseP parseE =
  do (input, feedback) <- try (parseInput parseP parseE <* parseDelimiter) <|> pure ([], [])
     lines <- many (parseVerboseLine parseP parseE)
     (outputs,returns) <- option ([], []) (parseDelimiter *> parseOutput parseE parseP)
     eof
     return $ Block input feedback lines outputs returns

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


module OpenGames.Preprocessor.FreeOpenGames where

-- A "free open game" is a abstract syntax tree for the code to build an open game
-- It uses 3 datatypes representing Haskell expressions used in different ways
-- Everything uses Show instances for code generation

import Data.List (intercalate)

-- Redefine types from AbstractSyntax as newtypes, for better type safety

tuple :: [String] -> String
tuple [x] = x
tuple xs = "(" ++ intercalate ", " xs ++ ")"

newtype VariableList   = VariableList {variableList :: [String]}
newtype ExpressionList = ExpressionList {expressionList :: [String]}

instance Show VariableList   where show = tuple . variableList
instance Show ExpressionList where show = tuple . expressionList

flattenVariableList :: [VariableList] -> VariableList
flattenVariableList = VariableList . concat . map variableList

-- Atom expressions are Haskell expressions used raw, intended for referring to existing open games

newtype AtomExpression = AtomExpression String

instance Show AtomExpression where
  show (AtomExpression e) = concat ["(", e, ")"]

-- Function expressions are Haskell expressions used as inputs to fromLens (from the class OG)

data FunctionExpression = Identity                               -- \x -> x
                        | Copy                                   -- \x -> (x, x)
                        | Lambda VariableList ExpressionList     -- \(x1, ..., xm) -> (e1, ..., en)
                        | CopyLambda VariableList ExpressionList -- \(x1, ..., xm) -> ((x1, ..., xm), (e1, ..., en))
                        | Multiplex VariableList VariableList    -- \((x1, ..., xm), (y1, ..., yn)) -> (x1, ..., xm, y1, ..., yn)
                        | Curry FunctionExpression               -- curry f

instance Show FunctionExpression where
  show Identity         = "\\x -> x"
  show Copy             = "\\x -> (x, x)"
  show (Lambda x e)     = concat ["\\", show x, " -> ", show e]
  show (CopyLambda x e) = concat ["\\", show x, " -> (", show x, ", ", show e, ")"]
  show (Multiplex x y)  = concat ["\\(", show x, ", ", show y, ") -> ", show (flattenVariableList [x, y])]
  show (Curry f)        = concat ["curry (", show f, ")"]

-- Reindexing expressions are Haskell expressions used as inputs to reindex (from the class OG)

data ReindexingExpression = UnitIntroL        -- \x -> ((), x)
                          | UnitIntroR        -- \x -> (x, ())
                          | FlattenTuples Int -- \((...(x1, x2), ...), xn) -> (x1, x2, ..., xn)

instance Show ReindexingExpression where
  show UnitIntroL = "\\x -> ((), x)"
  show UnitIntroR = "\\x -> (x, ())"
  show (FlattenTuples n) = let vs = map (\n -> "a" ++ show n) [1 .. n]
                               x  = show (VariableList vs)
                               y  = foldl1 (\a b -> concat ["(", a, ", ", b, ")"]) vs
                            in concat ["\\", x, " -> ", y]

-- The main abstract datatype targeted by the compiler

data FreeOpenGame = Atom AtomExpression
                  | Lens FunctionExpression FunctionExpression
                  | Function FunctionExpression FunctionExpression
                  | Counit
                  | Sequential FreeOpenGame FreeOpenGame
                  | Simultaneous FreeOpenGame FreeOpenGame
                  | Reindex ReindexingExpression FreeOpenGame

instance Show FreeOpenGame where
  show (Atom e)           = show e
  show (Lens v u)         = concat ["fromLens (", show v, ") (", show u, ")"]
  show (Function f g)     = concat ["fromFunctions (", show f, ") (", show g, ")"]
  show Counit             = "counit"
  show (Sequential g h)   = concat ["(", show g, ") >>> (", show h, ")"]
  show (Simultaneous g h) = concat ["(", show g, ") &&& (", show h, ")"]
  show (Reindex e g)      = concat ["reindex (", show e, ") (", show g, ")"]

-- Macros commonly used in compiled code

sequentialTrivialL, sequentialTrivialR, simultaneousTrivialL, simultaneousTrivialR :: FreeOpenGame -> FreeOpenGame -> FreeOpenGame
sequentialTrivialL g h   = Reindex UnitIntroL (Sequential g h)
sequentialTrivialR g h   = Reindex UnitIntroR (Sequential g h)
simultaneousTrivialL g h = Reindex UnitIntroL (Simultaneous g h)
simultaneousTrivialR g h = Reindex UnitIntroR (Simultaneous g h)

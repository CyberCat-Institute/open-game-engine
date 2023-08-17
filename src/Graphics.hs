{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Graphics where

import Control.Lens.Combinators
import Data.Data.Lens
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz as GV hiding (Both)
import Data.Maybe
import Debug.Trace
import Language.Haskell.TH
import OpenGames.Preprocessor.BlockSyntax

freshLabel :: [String] -> [LNode String]
freshLabel = freshLabelsStateful 0
  where
    freshLabelsStateful :: Int -> [String] -> [LNode String]
    freshLabelsStateful curr [] = []
    freshLabelsStateful curr (n : ns) = (curr, n) : freshLabelsStateful (curr + 1) ns

-- | A Data type to store tell if an edge label is a covariant or contravariant arrow in the graph
data ArrowType = Contravariant String | Covariant String | Both String
  deriving (Show, Eq)

-- | ArrowTypes are labellable using the string they wrap
instance Labellable ArrowType where
  toLabelValue (Contravariant s) = toLabelValue s
  toLabelValue (Covariant s) = toLabelValue s
  toLabelValue (Both x) = undefined

-- | An expression contains the name `Name` if any of the constructor of `Exp` uses it
containsName :: Name -> Exp -> Bool
containsName nm exp = trace ("searching for " ++ show nm ++ " in expression " ++ show exp) $ anyOf (template @Exp @Name) (== nm) exp

-- | Swap the two elements of a pair
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

isOpposite :: ArrowType -> ArrowType -> Bool
isOpposite (Covariant _) (Contravariant _) = True
isOpposite (Contravariant _) (Covariant _) = True
isOpposite _ _ = False

getName :: ArrowType -> String
getName (Covariant nm) = nm
getName (Contravariant nm) = nm
getName (Graphics.Both nm) = nm

remove :: Int -> [a] -> [a]
remove 0 (x : xs) = xs
remove n (y : ys) = y : remove (n - 1) ys
remove _ _ = error "cannot remove from an empty list"

convertBoth :: (a -> a -> Bool) -> (a -> a) -> [a] -> [a]
convertBoth test map [] = []
convertBoth test map (a : as) = case ifind (const $ test a) as of
  Just (idx, val) -> map val : convertBoth test map (remove idx as)
  Nothing -> a : convertBoth test map as

-- | Return all the edges from one `Line`
getEdgesFromName ::
  -- | The name of the line
  Name ->
  -- | The id of the line
  Int ->
  -- | The list of all the other lines
  [Line (String, Int) Pat Exp] ->
  -- | The list of edges from the line with given name and id to all the other lines
  [LEdge ArrowType]
getEdgesFromName name id lines =
  [ (id, snd (label ln), Contravariant (show name))
    | ln <- lines,
      any (containsName name) ((contravariantInputs ln))
  ]
    ++ [ (id, snd (label ln), Covariant (show name))
         | ln <- lines,
           any (containsName name) ((covariantInputs ln))
       ]

-- | Get the edges for all lines by looking at each covariant and contravariant outputs and mapping it to the list
-- of lines which references them.
getEdges :: [Line (String, Int) Pat Exp] -> Line (String, Int) Pat Exp -> [LEdge ArrowType]
getEdges allLines line =
  let outputs = traverse getName (covariantOutputs line ++ contravariantOutputs line)
      labels = concat [getEdgesFromName x (snd . label $ line) allLines | x <- fromMaybe [] outputs]
   in labels
  where
    getName :: Pat -> Maybe Name
    getName (VarP n) = Just n
    getName _ = Nothing

-- | Convert a line with optional labels to lines with a label and an id
-- if the line has no label the label will be `line_id_n` where `n` is the id of the line
convertLines :: [Line (Maybe String) Pat Exp] -> [Line (String, Int) Pat Exp]
convertLines ln = convertLinesRec 0 ln
  where
    convertLinesRec :: Int -> [Line (Maybe String) Pat Exp] -> [Line (String, Int) Pat Exp]
    convertLinesRec n [] = []
    convertLinesRec n (Line nm a b c d e : ls) = Line (fromMaybe ("line_id_" ++ show n) nm, n) a b c d e : convertLinesRec (n + 1) ls

-- | Build a graph using lines as nodes and argument dependency for nodes
toGraph :: [Line (String, Int) Pat Exp] -> Gr String ArrowType
toGraph ln =
  let edges = concat $ map (getEdges ln) ln
   in mkGraph (map (swap . label) ln) (trace ("edges are " ++ show edges) edges)

-- | Given a block, convert it into a graph using all lines as nodes and dependencies between them as arrows
convertBlock :: Block Pat Exp -> Gr String ArrowType
convertBlock = toGraph . convertLines . blockLines

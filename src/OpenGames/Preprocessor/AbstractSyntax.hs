module OpenGames.Preprocessor.AbstractSyntax where

-- The user interacts with the preprocessor by creating instances of the datatypes in this file
-- and then calling functions from Compiler on it

-- The only reason there is no concrete syntax is that I have no idea how to write a parser
-- Somebody can probably fix that in half an hour
-- My idea for the concrete syntax of a line is
-- cvo, ..., cvo' | cno, ..., cno' <- matrix -< cvi, ..., cvi' | cni, ..., cvi'

-- covariant input = X, covariant output = Y, contravariant input = R, contravariant output = S

-- There is an important duality that the types can't express: half of these are lists of Haskell variables
-- (they could probably be patterns) that create new bindings, and half of them are lists of Haskell expressions
-- Line outputs and block inputs are variables/patterns, line inputs and block outputs are expressions

-- Variables/patterns: covariantOutput, contravariantOutput, blockCovariantInput, blockContravariantInput
-- Expressions:        covariantInput, contravariantInput, blockCovariantOutput, blockContravariantOutput

-- I decided to keep the record field names verbose, and I expect the user to specify lines in constructor syntax
-- rather than record syntax

data Line = Line {
  covariantInputs :: [String], contravariantOutputs :: [String],
  matrix :: String,
  covariantOutputs :: [String], contravariantInputs :: [String]}

data Block = Block {
  blockCovariantInputs :: [String], blockContravariantOutputs :: [String],
  blockLines :: [Line],
  blockCovariantOutputs :: [String], blockContravariantInputs :: [String]}


{-# LANGUAGE TemplateHaskell #-}
module Example where


import Data.FileEmbed
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Validation
import Error

import Parse
import Lex
import Syntax.Annotated
import CLI

actSource :: Data.ByteString.ByteString
actSource = $(embedFile "amm.act")

compiled :: Error String [Claim]
compiled =  compile (unpack actSource)

display ::  String
display = case compiled of
  Success s -> unlines (fmap printClaim s)

printClaim :: Claim -> String
printClaim (C constructor) = "constructor"
printClaim (B b) = "behaviour: " ++ unlines (fmap show (_preconditions b))
printClaim (I i) = "invariant: " ++ show i
printClaim (S s) = "Storage: " ++ show s

printConstructor :: Constructor -> String
printConstructor (Constructor
  name
  mode
  interface
  preconditions
  postconditions
  initialStorage
  stateupdates) =
  "constructor: " ++ name

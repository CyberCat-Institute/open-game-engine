{-# LANGUAGE TemplateHaskell #-}
module Example where


import Data.FileEmbed
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Validation

import Parse
import Lex
import Syntax.Annotated
import CLI

actSource :: Data.ByteString.ByteString
actSource = $(embedFile "amm.act")

compiled =  compile (unpack actSource)

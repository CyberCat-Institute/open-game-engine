{-# LANGUAGE TemplateHaskell #-}
module Example where

import Act.TH


$(act2OG "amm.act")



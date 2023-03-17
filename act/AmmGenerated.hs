{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module AmmGenerated where

import Act.TH
import Act.Prelude
import Act.Execution

$(act2OG "amm.act")

twoAmms = combine (unionContracts ("amm1", ammContract) ("amm2", ammContract))

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Examples.AmmGenerated where

import Act.TH
import Act.Prelude
import Act.Execution

-- This generates the `ammContract`
$(act2OG "amm.act")

-- This combines two contracts with non-shared state
twoAmms = combine (unionContracts ("amm1", ammContract) ("amm2", ammContract))

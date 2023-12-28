{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Examples.AmmGenerated where

import Act.TH

-- This generates the `ammContract`
$(act2OG "act-programs/amm.act")

-- $(hevm2OG "contract") --- do something???

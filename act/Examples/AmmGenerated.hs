{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Examples.AmmGenerated where

import Act.TH

-- This generates the `ammContract`
$(act2OG "amm.act")

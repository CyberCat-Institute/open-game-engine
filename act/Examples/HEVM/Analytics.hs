{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Examples.HEVM.Analytics
   where

import Examples.HEVM.Model

import OpenGames.Engine.Engine hiding (Payoff)
import OpenGames.Preprocessor


import qualified Control.Monad.State  as ST
import qualified Numeric.Probability.Distribution as P

{-
Contains the basic analytics to be executed
There are two type of analyses:
1. (Bayesian) Nash eq. checks
2. Simulations
-}

--------------------------
-- 1. Equilibrium checking
--------------------------

-----------------
-- 2. Simulations
-----------------



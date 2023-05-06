{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Criterion
import Criterion.Main
import Data.Maybe
import qualified Examples.Markov.RepeatedPD as RPD
import qualified Examples.Markov.TestSimpleMonteCarlo as MC
import Examples.SimultaneousMoves (ActionPD (..), prisonersDilemmaMatrix)
import System.Environment
import Text.Read

main = do
  iters <- fmap (fromMaybe [5, 6, 7] . (>>= readMaybe)) (lookupEnv "ITERS")
  defaultMain
    [ bgroup
        "Old version"
        [ bench
            ("iters/" ++ show i)
            (nfIO (do RPD.eqOutput i RPD.strategyTupleTest (Cooperate, Cooperate)))
          | i <- iters
        ],
      bgroup
        "Monte Carlo version"
        [ bench
            ("iters/" ++ show i)
            (nfIO (do MC.eqOutput 1000 i MC.strategyTupleTest (Cooperate, Cooperate)))
          | i <- iters
        ]
    ]

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyCase, DuplicateRecordFields, TemplateHaskell, QuasiQuotes, GADTs #-}



module Main where


import           Criterion
import           Criterion.Main
import           Data.Maybe
import           System.Environment
import           Text.Read

import qualified Examples.Markov.TestSimpleMonteCarlo as MC
import qualified Examples.Markov.RepeatedPD           as RPD
import           Examples.SimultaneousMoves (ActionPD(..),prisonersDilemmaMatrix)

main = do
  iters <-  fmap (fromMaybe [5,6,7] . (>>=readMaybe)) (lookupEnv "ITERS")
  defaultMain
    [ bgroup
        "Old version"
        [ bench
          ("iters/" ++ show i)
          (nfIO (do RPD.eqOutput i RPD.strategyTupleTest (Cooperate,Cooperate)))
        | i <- iters
        ]
    , bgroup
        "Monte Carlo version"
        [ bench
          ("iters/" ++ show i)
          (nfIO (do MC.eqOutput 1000 i MC.strategyTupleTest (Cooperate,Cooperate)))
        | i <- iters
        ]]

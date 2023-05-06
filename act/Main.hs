{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Act.Execution
import Act.Prelude
import Act.TH
-- import Examples.Amm

import EVM.ABI
import Examples.AmmGenerated
import Examples.Player
import Numeric.Probability.Distribution
import OpenGames.Engine.Engine
import OpenGames.Engine.OpenGames
import OpenGames.Preprocessor

-- questions:
-- - What do we improve in this model next?
--   - sandwich? (different example)
--   - another "from act" example
--   - betting contract from act?
--     - betting on the exchange rate of an AMM
--     - clockwork finance example
--     - move on to token swap
-- - What do we automate from Act ?
--   - extract name state fields
--   - what about rollback?
--   - strategy stealing?
--   - failing transactions added to the global state?
--   - generate players ?
--   - gas fees + mem pool + Bribable coordinator
--
--   To do in general:
--   - Work on the common infrastructure around modelling situations
--     - coordinator
--     - calling subcontract
--     - bribes
--
--   Next week:
--   - Work on another act example, maybe draw from clockwork finance
--     - Create an act program for a full AMM with setup
--     - Create an act program for betting
--
--  ## 10.03
--  - We have a game with multiple AMM and a way to dispatch transactions
--  todo:
--  - send multiple transactions and check they are executed correctly
--  - game to find which transaction order would optimise the payoff
--      - run this for 1 amm
--  - Work toward having common state between AMMs
--  - same operations but now find how to

ctx =
  StochasticStatefulContext @()
    (pure ((), (AmmState 8 10, AmmState 10 8)))
    (\_ _ -> return ())

ev = evaluate (playerAutomatic 10) ((pureAction 1) :- Nil) ctx

ctx1 =
  StochasticStatefulContext @()
    (pure ((), (AmmState 10 10)))
    (\_ _ -> return ())

ev1 = evaluate (swapSequence) ((pureAction (reverse allTransaction)) :- Nil) ctx1

main :: IO ()
main = putStrLn "hello Act"

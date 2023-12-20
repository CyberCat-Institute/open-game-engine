{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.Prisoner where

import Control.Monad.Trans.State.Strict (evalStateT)
import EVM.TH
import EVM.Prelude
import EVM.Stepper (evm, interpret, runFully)
import EVM.Fetch (zero)
import OpenGames hiding (dependentDecision, fromFunctions, fromLens)
import OpenGames.Engine.HEVMGames
import OpenGames.Preprocessor hiding (Lit)

$(loadAll [ContractInfo "solidity/Prisonner.sol" "Prison" "prison"])

player1 = LitAddr 0x1234

player2 = LitAddr 0x1235

bank = LitAddr 0x1236

p1defect = prison_defect player1  0 10_000_000

p2defect = prison_defect player2  0 10_000_000

p1coop = prison_cooperate player1 0 10_000_000

p2coop = prison_cooperate player2 0 10_000_000

contractFail = EthTransaction (LitAddr 0x1000) bank "deposit()" [] 10000 10_000_000

-- each player can either cooperate or defect
optionsPlayer1 =
  [ prison_defect player1 0 10_000_000,
    prison_cooperate player1 0 10_000_000
  ]

optionsPlayer2 =
  [ prison_defect player2 0 10_000_000,
    prison_cooperate player2 0 10_000_000
  ]

sendAndRunBoth (a, b) = sendAndRunAll [a, b]

hevmDilemma =
  [opengame|
  inputs : ;
  feedback : ;
  :-----------:

  operation : hevmDecision "player1" optionsPlayer1 ;
  outputs : decisionP1 ;
  returns : balance finalState player1 ;

  operation : hevmDecision "player2" optionsPlayer2 ;
  outputs : decisionP2 ;
  returns : balance finalState player2 ;

  inputs : decisionP1, decisionP2 ;
  feedback : ;
  operation : fromLensM sendAndRunBoth (const pure);
  outputs : finalState ;
  returns : ;

  :-----------:
  outputs: ;
  returns: ;
  |]

outcomeAutomatic = do
  let addresses =
        [ (player1, Lit 1_000_000_000),
          (player2, Lit 1_000_000_000),
          (bank, Lit 1_000_000_000)
          -- (LitAddr 0x1000, Lit 1_000_000_000)
        ]
  i <- setupAddresses addresses <$> stToIO initial
  i <- interpret (zero 0 (Just 0)) i (evm (makeTxCall contractFail) >> runFully)
  let aaa :- bbb :- Nil = evaluate hevmDilemma (const p1coop :- const p2coop :- Nil) void
  evaluated1 <- stToIO (evalStateT aaa i)
  evaluated2 <- stToIO (evalStateT bbb i)
  generateOutput (evaluated1 :- evaluated2 :- Nil)


execManually = do

  let addresses =
        [ (player1, Lit 1_000_000_000),
          (player2, Lit 1_000_000_000)
          -- (LitAddr 0x1000, Lit 2_000_000_000)
        ]
  init <- stToIO initial
  putStrLn "initial contracts:"
  print $ getAllContracts init
  let i = setupAddresses addresses init
  -- out <- stToIO $ evalStateT (sendAndRun' contractFail) i
  -- out <- stToIO $ evalStateT (sendAndRunAll [p1defect, p2defect, p1coop, p2coop]) i
  putStrLn "setup contracts:"
  print $ getAllContracts i
  out <- interpret (zero 0 (Just 0)) i (evm (makeTxCall contractFail) >> runFully)
  putStrLn "end contracts:"
  print $ getAllContracts out
  -- out <- interpret (zero 0 (Just 0)) i
  --   (  evm (makeTxCall (EthTransaction (LitAddr 0x1000) player1 "defect()" [] 0 10_000_000))
  --   >> runFully
  --   >> evm (makeTxCall (EthTransaction (LitAddr 0x1000) player2 "defect()" [] 0 10_000_000))
  --   >> runFully)
  let p1 = balance out player1
  let p2 = balance out player2
  let contract = balance out (LitAddr 0x1000)
  putStrLn $ "player1: " ++ show p1
  putStrLn $ "player2: " ++ show p2
  putStrLn $ "contract: " ++ show contract
  pure ()


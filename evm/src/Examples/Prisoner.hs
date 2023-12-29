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
import EVM.Fetch (zero)
import EVM.Prelude
import EVM.Stepper (evm, interpret, runFully)
import EVM.TH
import OpenGames hiding (dependentDecision, fromFunctions, fromLens)
import OpenGames.Engine.HEVMGames
import OpenGames.Preprocessor hiding (Lit)

$(loadAll [ContractInfo "solidity/Prisonner.sol" "Prison" "prison"])

player1 = LitAddr 0x1234

player2 = LitAddr 0x1235

p1defect = prison_defect player1 0 10_000_000

p2defect = prison_defect player2 0 10_000_000

p1coop = prison_cooperate player1 0 10_000_000

p2coop = prison_cooperate player2 0 10_000_000

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
          (LitAddr 0x1000, Lit 10_000)
        ]
  i <- setupAddresses addresses <$> stToIO initial
  let aaa :- bbb :- Nil = evaluate hevmDilemma (const p1coop :- const p2coop :- Nil) void
  evaluated1 <- stToIO (evalStateT aaa i)
  evaluated2 <- stToIO (evalStateT bbb i)
  let out1 = generateOutputStr (evaluated1 :- evaluated2 :- Nil)

  let aaa :- bbb :- Nil = evaluate hevmDilemma (const p1defect :- const p2coop :- Nil) void
  evaluated1 <- stToIO (evalStateT aaa i)
  evaluated2 <- stToIO (evalStateT bbb i)
  let out2 = generateOutputStr (evaluated1 :- evaluated2 :- Nil)
  let aaa :- bbb :- Nil = evaluate hevmDilemma (const p1coop :- const p2defect :- Nil) void
  evaluated1 <- stToIO (evalStateT aaa i)
  evaluated2 <- stToIO (evalStateT bbb i)
  let out3 = generateOutputStr (evaluated1 :- evaluated2 :- Nil)
  let aaa :- bbb :- Nil = evaluate hevmDilemma (const p1defect :- const p2defect :- Nil) void
  evaluated1 <- stToIO (evalStateT aaa i)
  evaluated2 <- stToIO (evalStateT bbb i)

  let out4 = generateOutputStr (evaluated1 :- evaluated2 :- Nil)
  pure (out1 ++ out2 ++ out3 ++ out4)

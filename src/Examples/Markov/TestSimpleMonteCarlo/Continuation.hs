{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Examples.Markov.TestSimpleMonteCarlo.Continuation
  ( sampleDetermineContinuationPayoffsStoch,
    discountFactor,
  )
where

import Control.Monad.State hiding (lift, state, void)
import qualified Control.Monad.State as ST
import Data.Utils
import qualified Data.Vector as V
import Debug.Trace
import Examples.SimultaneousMoves (ActionPD (..), prisonersDilemmaMatrix)
import Numeric.Probability.Distribution hiding (filter, lift, map)
import OpenGames hiding (Agent, discount, fromFunctions, fromLens)
import OpenGames.Engine.IOGames
import OpenGames.Preprocessor
import System.IO.Unsafe
import System.Random
import System.Random.MWC.CondensedTable
import System.Random.Stateful

---------------------------------------------
-- Contains a first, very, very shaky version
-- that does Monte Carlo through the evaluate
---------------------------------------------

discountFactor = 1.0

prisonersDilemmaCont ::
  OpenGame
    MonadOptic
    MonadContext
    ( '[ Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD,
         Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD
       ]
    )
    ('[IO (DiagnosticsMC ActionPD), IO (DiagnosticsMC ActionPD)])
    (ActionPD, ActionPD)
    ()
    (ActionPD, ActionPD)
    ()
prisonersDilemmaCont =
  [opengame|

   inputs    : (dec1Old,dec2Old) ;
   feedback  :      ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecisionIO "player1" 100 [Cooperate,Defect];
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecisionIO "player2" 100 [Cooperate,Defect];
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   :      ;
  |]

transformStrat :: Kleisli Stochastic x y -> Kleisli CondensedTableV x y
transformStrat strat =
  Kleisli
    ( \x ->
        let y = runKleisli strat x
            ls = decons y
            v = V.fromList ls
         in tableFromProbabilities v
    )

transformStratTuple ::
  List
    '[ Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
       Kleisli Stochastic (ActionPD, ActionPD) ActionPD
     ] ->
  List
    '[ Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD,
       Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD
     ]
transformStratTuple (x :- y :- Nil) =
  transformStrat x
    :- transformStrat y
    :- Nil

-- extract continuation
extractContinuation :: MonadOptic s () a () -> s -> StateT Vector IO ()
extractContinuation (MonadOptic v u) x = do
  (z, a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: MonadOptic s () a () -> s -> IO a
extractNextState (MonadOptic v _) x = do
  (z, a) <- v x
  pure a

executeStrat strat = play prisonersDilemmaCont strat

--------------------------------
-- This is for the mixed setting
-- which includes the Bayesian setup
-- determine continuation for iterator, with the same repeated strategy
determineContinuationPayoffs ::
  Integer ->
  List
    '[ Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
       Kleisli Stochastic (ActionPD, ActionPD) ActionPD
     ] ->
  (ActionPD, ActionPD) ->
  StateT Vector IO ()
determineContinuationPayoffs 1 strat action = pure ()
determineContinuationPayoffs iterator strat action = do
  extractContinuation executeStrat action
  nextInput <- ST.lift $ extractNextState executeStrat action
  determineContinuationPayoffs (pred iterator) strat nextInput
  where
    executeStrat = play prisonersDilemmaCont (transformStratTuple strat)

sampleDetermineContinuationPayoffs ::
  -- | Sample size
  Int ->
  -- | How many rounds are explored?
  Integer ->
  List
    '[ Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
       Kleisli Stochastic (ActionPD, ActionPD) ActionPD
     ] ->
  (ActionPD, ActionPD) ->
  StateT Vector IO ()
sampleDetermineContinuationPayoffs sampleSize iterator strat initialValue = do
  replicateM_ sampleSize (determineContinuationPayoffs iterator strat initialValue)
  v <- ST.get
  ST.put (average sampleSize v)

-- NOTE EVIL EVIL
sampleDetermineContinuationPayoffsStoch ::
  -- | Sample size
  Int ->
  -- | How many rounds are explored?
  Integer ->
  List
    '[ Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
       Kleisli Stochastic (ActionPD, ActionPD) ActionPD
     ] ->
  (ActionPD, ActionPD) ->
  StateT Vector Stochastic ()
sampleDetermineContinuationPayoffsStoch sampleSize iterator strat initialValue = do
  transformStateTIO $ sampleDetermineContinuationPayoffs sampleSize iterator strat initialValue
  where
    transformStateTIO :: StateT Vector IO () -> StateT Vector Stochastic ()
    transformStateTIO sTT = StateT (\s -> unsafeTransformIO $ ST.runStateT sTT s)
    unsafeTransformIO :: IO a -> Stochastic a
    unsafeTransformIO a =
      let a' = unsafePerformIO a
       in certainly a'

-----------------------------
-- For IO only
-- determine continuation for iterator, with the same repeated strategy
determineContinuationPayoffsIO ::
  Integer ->
  List
    [ Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD,
      Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD
    ] ->
  (ActionPD, ActionPD) ->
  StateT Vector IO ()
determineContinuationPayoffsIO 1 strat action = pure ()
determineContinuationPayoffsIO iterator strat action = do
  extractContinuation executeStrat action
  nextInput <- ST.lift $ extractNextState executeStrat action
  determineContinuationPayoffsIO (pred iterator) strat nextInput
  where
    executeStrat = play prisonersDilemmaCont strat

-- fix context used for the evaluation
contextCont iterator strat initialAction = MonadContext (pure ((), initialAction)) (\_ action -> determineContinuationPayoffsIO iterator strat action)

repeatedPDEq iterator strat initialAction = evaluate prisonersDilemmaCont strat context
  where
    context = contextCont iterator strat initialAction

printOutput iterator strat initialAction = do
  let (result1 :- result2 :- Nil) = repeatedPDEq iterator strat initialAction
  result1' <- result1
  result2' <- result2
  putStrLn "Player1"
  print result1'
  putStrLn "Player2"
  print result2'

----------------------------------------------------
-- This is taken from the other MonteCarloTest setup
-- Needs to be transformed in order to be tested

-- Add strategy for stage game
stageStrategy :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategy =
  Kleisli $
    ( \case
        (Cooperate, Cooperate) -> playDeterministically Cooperate
        (_, _) -> playDeterministically Defect
    )

-- Stage strategy tuple
strategyTuple = stageStrategy :- stageStrategy :- Nil

-- Testing for stoch behavior and slow down
stageStrategyTest :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD
stageStrategyTest = Kleisli $ const $ distFromList [(Cooperate, 0.5), (Defect, 0.5)]

-- Stage strategy tuple
strategyTupleTest = stageStrategyTest :- stageStrategyTest :- Nil

-- Example usage:
{--
printOutput 20 (transformStratTuple strategyTupleTest) (Cooperate,Cooperate)
Own util 1
45.296
Other actions
[43.957,45.309]
Own util 2
44.944
Other actions
[44.415,45.746]-}

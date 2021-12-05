{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Examples.Markov.TestSimpleMonteCarlo.Continuation where

import           Engine.Engine hiding (fromLens,Agent,fromFunctions,discount)
import           Preprocessor.Preprocessor
import           Examples.SimultaneousMoves (ActionPD(..),prisonersDilemmaMatrix)
import           Engine.IOGames
import           Data.Utils

import           Control.Monad.State  hiding (state,void)
import qualified Control.Monad.State  as ST
import qualified Data.Vector as V
import           Debug.Trace
import System.Random.MWC.CondensedTable
import System.Random
import System.Random.Stateful
import Numeric.Probability.Distribution hiding (map, lift, filter)

-- TODO I probably need to kill the state hack in order to do proper averaging


discountFactor = 0.9

prisonersDilemmaCont :: OpenGame
                          MonadOptic
                          MonadContext
                          ('[Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD,
                             Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD])
                          ('[])
                          (ActionPD, ActionPD)
                          ()
                          (ActionPD, ActionPD)
                          ()

prisonersDilemmaCont = [opengame|

   inputs    : (dec1Old,dec2Old) ;
   feedback  :      ;

   :----------------------------:
   inputs    :  (dec1Old,dec2Old)    ;
   feedback  :      ;
   operation : dependentDecisionIO "player1";
   outputs   : decisionPlayer1 ;
   returns   : prisonersDilemmaMatrix decisionPlayer1 decisionPlayer2 ;

   inputs    :   (dec1Old,dec2Old)   ;
   feedback  :      ;
   operation : dependentDecisionIO "player2";
   outputs   : decisionPlayer2 ;
   returns   : prisonersDilemmaMatrix decisionPlayer2 decisionPlayer1 ;

   operation : discount "player1" (\x -> x * discountFactor) ;

   operation : discount "player2" (\x -> x * discountFactor) ;

   :----------------------------:

   outputs   : (decisionPlayer1,decisionPlayer2)     ;
   returns   :      ;
  |]


transformStrat :: Kleisli Stochastic (ActionPD, ActionPD) ActionPD -> Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD
transformStrat strat = Kleisli (\x ->
  let y = runKleisli strat x
      ls = decons y
      v = V.fromList ls
      in tableFromProbabilities v)


transformStratTuple :: List
                        '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                          Kleisli Stochastic (ActionPD, ActionPD) ActionPD]
                    -> List
                        '[Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD,
                          Kleisli CondensedTableV (ActionPD, ActionPD) ActionPD]
transformStratTuple (x ::- y ::- Nil) =
  transformStrat x
  ::- transformStrat y
  ::- Nil




-- extract continuation
extractContinuation :: MonadOptic s () a () -> s -> StateT Vector IO ()
extractContinuation (MonadOptic v u) x = do
  (z,a) <- ST.lift (v x)
  u z ()

-- extract next state (action)
extractNextState :: MonadOptic s () a () -> s -> IO a
extractNextState (MonadOptic v _) x = do
  (z,a) <- v x
  pure a

executeStrat strat =  play prisonersDilemmaCont strat


-- determine continuation for iterator, with the same repeated strategy
determineContinuationPayoffs :: Integer
                             -> List
                                      '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                                        Kleisli Stochastic (ActionPD, ActionPD) ActionPD]
                             -> (ActionPD,ActionPD)
                             -> StateT Vector IO ()
determineContinuationPayoffs 1        strat action = pure ()
determineContinuationPayoffs iterator strat action = do
   extractContinuation executeStrat action
   nextInput <- ST.lift $ extractNextState executeStrat action
   determineContinuationPayoffs (pred iterator) strat nextInput
 where executeStrat =  play prisonersDilemmaCont (transformStratTuple strat)


sampleDetermineContinuationPayoffs :: Int
                                  -- ^ Sample size
                                  -> Integer
                                  -- ^ How many rounds are explored?
                                  -> List
                                            '[Kleisli Stochastic (ActionPD, ActionPD) ActionPD,
                                              Kleisli Stochastic (ActionPD, ActionPD) ActionPD]
                                  -> (ActionPD,ActionPD)
                                  -> StateT Vector IO ()
sampleDetermineContinuationPayoffs sampleSize iterator strat initialValue = do
  replicateM_ sampleSize (determineContinuationPayoffs iterator strat initialValue)
  v <- ST.get
  ST.put (average sampleSize v)


-- TODO transform StateT Vector with IO
-- TODO check out https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html#v:hoist

transformStateTIO ::  StateT Vector IO () ->  StateT Vector Stochastic ()
transformStateTIO s = do
  v <- ST.get
  ST.put v

transformStateTIO2 ::  StateT Vector IO () ->  StateT Vector Stochastic ()
transformStateTIO2 x = StateT
  (\s -> (do
         v <- ST.runStateT x $ s
         pure  v))

mapStateT2 :: (m ((), s) -> n ((), s))
mapStateT2 s

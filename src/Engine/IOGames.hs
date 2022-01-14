{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.IOGames
  ( IOOpenGame(..)
  , Agent(..)
  , DiagnosticsMC(..)
  , dependentDecisionIO
  , fromLens
  , fromFunctions
  , discount
  ) where

import           Debug.Trace

import           Control.Arrow                      hiding ((+:+))
import           Control.Monad.Bayes.Weighted
import           Control.Monad.State                hiding (state)
import           Control.Monad.Trans.Class
import           Data.Foldable
import           Data.HashMap                       as HM hiding (null,map,mapMaybe)
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import           Data.Utils
import qualified Data.Vector as V
import           GHC.TypeLits
import           Numeric.Probability.Distribution hiding (map, lift)
import           System.Random.MWC.CondensedTable
import           System.Random
import           System.Random.Stateful

import           Engine.OpenGames hiding (lift)
import           Engine.OpticClass
import           Engine.TLL
import           Engine.Diagnostics


-- TODO implement the sampler
-- TODO implement printout

type IOOpenGame a b x s y r = OpenGame MonadOptic MonadContext a b x s y r

type Agent = String


data DiagnosticsMC y = DiagnosticsMC {
  playerNameMC :: String
  , averageUtilStrategyMC :: Double
  , samplePayoffsMC :: [Double]
  , optimalMoveMC :: y
  , optimalPayoffMC :: Double
  }
  deriving (Show)

-- NOTE This ignores the state
dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String -> Int -> [y] ->  IOOpenGame '[Kleisli CondensedTableV x y] '[IO (DiagnosticsMC y)] x () y Double
          -- s t  a b
-- ^ (average utility of current strategy, [average utility of all possible alternative actions])
dependentDecisionIO name sampleSize ys = OpenGame { play, evaluate} where
  -- ^ ys is the list of possible actions
  play = \(strat ::- Nil) -> let v x = do
                                   g <- newStdGen
                                   gS <- newIOGenM g
                                   action <- genFromTable (runKleisli strat x) gS
                                   return ((),action)
                                 u () r = modify (adjustOrAdd (+ r) r name)
                             in MonadOptic v u

  evaluate (strat ::- Nil) (MonadContext h k) =
    output ::- Nil

    where

      output = do
        zippedLs <- samplePayoffs
        let samplePayoffs' = map snd zippedLs
        let (optimalPlay, optimalPayoff0) = maximumBy (comparing snd) zippedLs
        (currentMove, averageUtilStrategy') <- averageUtilStrategy
        return  DiagnosticsMC{
            playerNameMC = name
          , averageUtilStrategyMC = averageUtilStrategy'
          , samplePayoffsMC = samplePayoffs'
          , optimalMoveMC = optimalPlay
          , optimalPayoffMC = optimalPayoff0
          }

        where
           action = do
              (_,x) <- h
              g <- newStdGen
              gS <- newIOGenM g
              genFromTable (runKleisli strat x) gS
           u y     = do
              (z,_) <- h
              evalStateT (do
                             r <- k z y
                           -- ^ utility <- payoff function given other players strategies and my own action y
                             gets ((+ r) . HM.findWithDefault 0.0 name))
                          HM.empty
           -- Sample the average utility from current strategy
           averageUtilStrategy = do
             (_,x) <- h
             actionLS' <- replicateM sampleSize action
             utilLS  <- mapM u actionLS'
             let average = (sum utilLS / fromIntegral sampleSize)
             return (x,average)
           -- Sample the average utility from a single action
           sampleY y = do
                  ls1 <- replicateM sampleSize (u y)
                  let average =  (sum ls1 / fromIntegral sampleSize)
                  pure (y, average)
           -- Sample the average utility from all actions
           samplePayoffs  = mapM sampleY ys



-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> IOOpenGame '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> MonadOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: (x -> y) -> (r -> s) -> IOOpenGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)



-- discount Operation for repeated structures
discount :: String -> (Double -> Double) -> IOOpenGame '[] '[] () () () ()
discount name f = OpenGame {
  play = \_ -> let v () = return ((), ())
                   u () () = modify (adjustOrAdd f (f 0) name)
                 in MonadOptic v u,
  evaluate = \_ _ -> Nil}

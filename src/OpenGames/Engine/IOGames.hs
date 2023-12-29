{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenGames.Engine.IOGames
  ( IOOpenGame (..),
    Agent (..),
    DiagnosticsMC (..),
    dependentDecisionIO,
    MonadOptic,
    MonadOpticM (..),
    MonadContext,
    MonadContextM (..),
    fromLens,
    fromLensM,
    fromFunctions,
    nature,
    discount,
  )
where

import Control.Arrow hiding ((+:+))
import Control.Monad (replicateM)
import Control.Monad.ST
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict hiding (state)
import Data.Foldable
import Data.HashMap as HM hiding (map, mapMaybe, null)
import Data.Ord (comparing)
import Data.Utils
import GHC.ST
import OpenGames.Engine.OpenGames hiding (lift)
import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL
import System.Random
import System.Random.MWC.CondensedTable
import System.Random.Stateful

-- TODO implement printout

type IOOpenGame a b x s y r = OpenGame MonadOptic MonadContext a b x s y r

type Agent = String

data DiagnosticsMC y = DiagnosticsMC
  { playerNameMC :: String,
    averageUtilStrategyMC :: Double,
    samplePayoffsMC :: [Double],
    optimalMoveMC :: y,
    optimalPayoffMC :: Double
  }
  deriving (Show)

-- NOTE This ignores the state
dependentDecisionIO ::
  (Eq x, Show x, Ord y, Show y) =>
  String ->
  Int ->
  [y] ->
  IOOpenGame '[Kleisli CondensedTableV x y] '[IO (DiagnosticsMC y)] x () y Double
-- s t  a b

-- ^ (average utility of current strategy, [average utility of all possible alternative actions])
dependentDecisionIO name sampleSize ys = OpenGame {play, evaluate}
  where
    -- \^ ys is the list of possible actions
    play = \(strat :- Nil) ->
      let v x = do
            g <- newStdGen
            gS <- newIOGenM g
            action <- genFromTable (runKleisli strat x) gS
            return ((), action)
          u () r = modify (adjustOrAdd (+ r) r name)
       in MonadOpticM v u

    evaluate (strat :- Nil) (MonadContextM h k) = output :- Nil
      where
        output = do
          zippedLs <- samplePayoffs
          let samplePayoffs' = map snd zippedLs
          let (optimalPlay, optimalPayoff0) = maximumBy (comparing snd) zippedLs
          (currentMove, averageUtilStrategy') <- averageUtilStrategy
          return
            DiagnosticsMC
              { playerNameMC = name,
                averageUtilStrategyMC = averageUtilStrategy',
                samplePayoffsMC = samplePayoffs',
                optimalMoveMC = optimalPlay,
                optimalPayoffMC = optimalPayoff0
              }
          where
            action = do
              (_, x) <- h
              g <- newStdGen
              gS <- newIOGenM g
              genFromTable (runKleisli strat x) gS
            u y = do
              (z, _) <- h
              evalStateT
                ( do
                    r <- k z y
                    -- \^ utility <- payoff function given other players strategies and my own action y
                    gets ((+ r) . HM.findWithDefault 0.0 name)
                )
                HM.empty
            -- Sample the average utility from current strategy
            averageUtilStrategy = do
              (_, x) <- h
              actionLS' <- replicateM sampleSize action
              utilLS <- mapM u actionLS'
              let average = (sum utilLS / fromIntegral sampleSize)
              return (x, average)
            -- Sample the average utility from a single action
            sampleY y = do
              ls1 <- replicateM sampleSize (u y)
              let average = (sum ls1 / fromIntegral sampleSize)
              pure (y, average)
            -- Sample the average utility from all actions
            samplePayoffs = mapM sampleY ys

-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> IOOpenGame '[] '[] x s y r
fromLens v u =
  OpenGame
    { play = \Nil -> MonadOpticM (\x -> return (x, v x)) (\x r -> return (u x r)),
      evaluate = \Nil _ -> Nil
    }

fromFunctions :: (x -> y) -> (r -> s) -> IOOpenGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)

fromLensM :: (x -> IO y) -> (x -> r -> IO s) -> IOOpenGame '[] '[] x s y r
fromLensM f g =
  OpenGame
    { play = \Nil -> MonadOpticM (\x -> (x,) <$> f x) (\x r -> lift $ g x r),
      evaluate = \Nil _ -> Nil
    }

nature :: CondensedTableV x -> IOOpenGame '[] '[] () () x ()
nature table = OpenGame {play, evaluate}
  where
    play _ =
      MonadOpticM v u
      where
        v () = do
          g <- newStdGen
          gS <- newIOGenM g
          draw <- genFromTable table gS
          return ((), draw)
        u _ _ = return ()

    evaluate = \_ _ -> Nil

-- discount Operation for repeated structures
discount :: String -> (Double -> Double) -> IOOpenGame '[] '[] () () () ()
discount name f =
  OpenGame
    { play = \_ ->
        let v () = return ((), ())
            u () () = modify (adjustOrAdd f (f 0) name)
         in MonadOpticM v u,
      evaluate = \_ _ -> Nil
    }

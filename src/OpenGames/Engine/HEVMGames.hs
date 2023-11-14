{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module OpenGames.Engine.HEVMGames where

import Control.Monad.Trans.State.Strict (StateT, execStateT, modify, evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad.ST
import Data.Foldable (maximumBy)
import Data.HashMap as HM hiding (map, mapMaybe, null)
import Data.Ord (comparing)
import Data.Utils
import OpenGames.Engine.OpenGames hiding (lift)
import OpenGames.Engine.OpticClass
import OpenGames.Engine.Diagnostics
import OpenGames.Engine.TLL
import OpenGames.Engine.Copy
import OpenGames.Engine.Diagnostics
import EVM.Types (VM)
import GHC.ST

import Debug.Trace

type OpenGameM m a b x s y r = OpenGame (MonadOpticM m) (MonadContextM m) a b x s y r

type HEVMState = StateT (VM RealWorld) (ST RealWorld)
type HEVMGame a b x s y r = OpenGameM HEVMState a b x s y r

hevmDecision :: forall x y . Show y => String -> [y] -> OpenGameM HEVMState '[x -> y] '[HEVMState (DiagnosticInfoBayesian x y)] x () y Double
hevmDecision name ys = OpenGame play eval
  where
    play :: List '[x -> y] -> MonadOpticM HEVMState x () y Double
    play (strat :- Nil) = MonadOpticM (\input -> pure ((), strat input))
                                      (\() payoff -> modify (adjustOrAdd (+ payoff) payoff name))


    eval :: List '[x -> y]
         -> MonadContextM (HEVMState) x () y Double -> List '[HEVMState (DiagnosticInfoBayesian x y)]
    eval (strat :- Nil) (MonadContextM h k) = output :- Nil
      where

        output :: HEVMState (DiagnosticInfoBayesian x y)
        output = do (residual, observation) <- h
                    let u y = do saveState <- copy
                                 payoff <- evalStateT (k residual y) HM.empty
                                 restore saveState
                                 pure payoff
                    let actualMove = strat observation
                    actualPayoff <- u actualMove
                    allResults <- traverse (\move -> (move,) <$> u move) ys
                    trace ("all results: " ++ unlines (fmap (\(mv, pf) -> show mv ++ ": " ++ show pf) allResults)) (pure ())
                    let (optimalMove, optimalPayoff) = maximumBy (comparing snd) allResults
                    return $ DiagnosticInfoBayesian
                           { equilibrium = actualPayoff == optimalPayoff,
                             player = name,
                             optimalMove = optimalMove,
                             strategy = pure (strat observation),
                             optimalPayoff = optimalPayoff,
                             context = error "impossible to implement",
                             payoff = actualPayoff,
                             state = observation,
                             unobservedState = "()"
                           }

fromLens :: (x -> y) -> (x -> r -> s) -> HEVMGame '[] '[] x s y r
fromLens v u =
  OpenGame
    { play = \Nil -> MonadOpticM (\x -> return (x, v x)) (\x r -> return (u x r)),
      evaluate = \Nil _ -> Nil
    }

fromFunctions :: (x -> y) -> (r -> s) -> HEVMGame '[] '[] x s y r
fromFunctions f g = fromLens f (const g)

fromLensM :: (x -> HEVMState y) -> (x -> r -> HEVMState s) -> HEVMGame '[] '[] x s y r
fromLensM f g =
  OpenGame
    { play = \Nil -> MonadOpticM (\x ->  (x,) <$> f x) (\x r -> lift $ g x r),
      evaluate = \Nil _ -> Nil
    }

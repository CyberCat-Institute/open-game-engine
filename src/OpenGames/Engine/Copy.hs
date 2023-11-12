{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
module OpenGames.Engine.Copy where


import Control.Monad.ST
import Control.Monad.Trans.State.Strict (StateT, put, get, modify)
import Data.Kind
import EVM.Types
import Data.Vector.Unboxed.Mutable

import Optics.Core
import Optics.State

class Copy (a :: Type -> Type) where
  copy :: StateT (a s) (ST s) (a s)

class Restore (a :: Type -> Type) where
  restore :: a s -> StateT (a s) (ST s) ()

instance Copy VM where
  copy = do state <- get
            let st = state ^. #state
            let fr = state ^. #frames
            st' <- copyFrameState st
            fr' <- traverse copyFrame fr
            let newState = state & #state .~ st'
                                 & #frames .~ fr'
                                 & #result .~ Nothing
            pure newState
            where

              copyFrame :: Frame s -> StateT (VM s) (ST s) (Frame s)
              copyFrame (Frame ctx state) = Frame ctx <$> copyFrameState state
              copyFrameState :: FrameState s -> StateT (VM s) (ST s) (FrameState s)
              copyFrameState oldFrame = do
                  let mem = oldFrame ^. #memory
                  mem' <- case mem of
                    ConcreteMemory mem -> ConcreteMemory <$> clone mem
                    SymbolicMemory x -> pure (SymbolicMemory x)

                  pure (oldFrame & #memory .~ mem')


instance Restore VM where
  restore = put

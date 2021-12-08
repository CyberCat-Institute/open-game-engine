{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Engine.IOGames
  ( IOOpenGame(..)
  , Agent(..)
  , dependentDecisionIO
  , fromLens
  , fromFunctions
  , discount
  ) where


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

-- Build game where one element is drawn
-- Ignore the Bayesian component for now
-- Build a sampler on top

type IOOpenGame a b x s y r = OpenGame MonadOptic MonadContext a b x s y r

type Agent = String



data DiagnosticInfoIO y = DiagnosticInfoIO
  { playerIO          :: String
  , optimalMoveIO     :: y
  , optimalPayoffIO   :: Double
  , currentMoveIO     :: y
  , currentPayoffIO   :: Double}

showDiagnosticInfoInteractive :: (Show y, Ord y) => DiagnosticInfoIO y -> String
showDiagnosticInfoInteractive info =
     "\n"    ++ "Player: " ++ playerIO info
     ++ "\n" ++ "Optimal Move: " ++ (show $ optimalMoveIO info)
     ++ "\n" ++ "Optimal Payoff: " ++ (show $ optimalPayoffIO info)
     ++ "\n" ++ "Current Move: " ++ (show $ currentMoveIO info)
     ++ "\n" ++ "Current Payoff: " ++ (show $ currentPayoffIO info)



-- output string information for a subgame expressions containing information from several players - bayesian
showDiagnosticInfoLIO :: (Show y, Ord y)  => [DiagnosticInfoIO y] -> String
showDiagnosticInfoLIO [] = "\n --No more information--"
showDiagnosticInfoLIO (x:xs)  = showDiagnosticInfoInteractive x ++ "\n --other game-- " ++ showDiagnosticInfoLIO xs


data PrintOutput = PrintOutput

instance (Show y, Ord y) => Apply PrintOutput [DiagnosticInfoIO y] String where
  apply _ x = showDiagnosticInfoLIO x


data Concat = Concat

instance Apply Concat String (String -> String) where
  apply _ x = \y -> x ++ "\n NEWGAME: \n" ++ y


---------------------
-- main functionality

-- all information for all players
generateOutputIO :: forall xs.
               ( MapL   PrintOutput xs     (ConstMap String xs)
               , FoldrL Concat String (ConstMap String xs)
               ) => List xs -> IO ()
generateOutputIO hlist = putStrLn $
  "----Analytics begin----" ++ (foldrL Concat "" $ mapL @_ @_ @(ConstMap String xs) PrintOutput hlist) ++ "----Analytics end----\n"





deviationsInContext :: (Show a, Ord a)
                    =>  Agent -> a -> (a -> IO Double) -> [a] -> IO [DiagnosticInfoIO a]
deviationsInContext name strategy u ys = do
     ls              <- mapM u ys
     strategicPayoff <- u strategy
     let zippedLs    =  zip ys ls
         (optimalPlay, optimalPayoff) = maximumBy (comparing snd) zippedLs
     pure [ DiagnosticInfoIO
            {  playerIO = name
            , optimalMoveIO = optimalPlay
            , optimalPayoffIO = optimalPayoff
            , currentMoveIO   = strategy
            , currentPayoffIO = strategicPayoff
            }]


-- NOTE This ignores the state
dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String -> [y] ->  IOOpenGame '[Kleisli CondensedTableV x y] '[IO [DiagnosticInfoIO y]] x () y Double
dependentDecisionIO name ys = OpenGame {
  play = \(strat ::- Nil) -> let v x = do
                                   g <- newStdGen
                                   gS <- newIOGenM g
                                   action <- genFromTable (runKleisli strat x) gS
                                   return ((),action)
                                 u () r = modify (adjustOrAdd (+ r) r name)
                             in MonadOptic v u,
  evaluate = \(strat ::- Nil) (MonadContext h k) -> do
       let action = do
              (_,x) <- h
              g <- newStdGen
              gS <- newIOGenM g
              genFromTable (runKleisli strat x) gS
           context = do
             action' <- action
             deviationsInContext name action' u ys
           u y     = do
              (z,_) <- h
              action' <- action
              evalStateT (do
                             r <- k z action'
                             gets ((+ r) . HM.findWithDefault 0.0 name))
                          HM.empty
              in (context  ::- Nil) }



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

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

module Engine.IOGames
  ( IOOpenGame(..)
  , Agent(..)
  , DiagnosticInfoIO(..)
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
dependentDecisionIO :: (Eq x, Show x, Ord y, Show y) => String -> Int -> [y] ->  IOOpenGame '[Kleisli CondensedTableV x y] '[IO (Double,[Double])] x () y Double
dependentDecisionIO name sampleSize ys = OpenGame {
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
           u y     = do
              (z,_) <- h
              evalStateT (do
                             r <- k z y --action'
                             gets ((+ r) . HM.findWithDefault 0.0 name))
                          HM.empty
           -- Sample the average utility from current strategy
           averageUtilStrategy = do
             actionLS' <- replicateM sampleSize action
             utilLS  <- mapM u actionLS'
             return (sum utilLS / fromIntegral sampleSize)
           -- Sample the average utility from a single action
           sampleY sampleSize y = do
                  ls1 <- replicateM sampleSize (u y)
                  pure  (sum ls1 / fromIntegral sampleSize)
           -- Sample the average utility from all actions
           samplePayoffs sampleSize = mapM (sampleY sampleSize) ys
           output = do
             samplePayoffs' <- samplePayoffs sampleSize
             averageUtilStrategy' <- averageUtilStrategy
             return $ (averageUtilStrategy', samplePayoffs')
              in (output ::- Nil) }



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

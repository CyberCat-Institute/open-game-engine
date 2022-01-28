{-# LANGUAGE DataKinds, NamedFieldPuns, DisambiguateRecordFields, LambdaCase, RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module OpenGames.Engine.IOGames
  ( IOOpenGame(..)
  , Agent(..)
  , dependentDecisionIO
  , fromLens
  , fromFunctions
  , discount
  , Msg(..)
  , PlayerMsg(..)
  , SamplePayoffsMsg(..)
  , AverageUtilityMsg(..)
  , DiagnosticsMC
  , logFuncSilent
  -- , logFuncTracing
  , logFuncStructured
  ) where


import           GHC.Stack
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Functor.Contravariant
import           Data.IORef
import qualified RIO
import           RIO (RIO, glog, GLogFunc, HasGLogFunc(..))

import           Control.Arrow hiding ((+:+))
import           Control.Monad.State hiding (state)
import           Data.HashMap as HM hiding (null,map,mapMaybe)
import           Data.List (maximumBy)
import           Data.Ord (comparing)
import           OpenGames.Data.Utils
import           System.Random.MWC.CondensedTable
import           System.Random
import           System.Random.Stateful

import           OpenGames.Engine.OpenGames hiding (lift)
import           OpenGames.Engine.OpticClass
import           OpenGames.Engine.TLL hiding ((++))
import           OpenGames.Engine.KleisliOptics

type Vector = HM.Map String Double

class Empty a where
instance Empty a where

type MonadOptic :: * -> * -> * -> * -> * -> *
type MonadOptic msg = KleisliOptic Empty (RIO (GLogFunc msg)) (StateT Vector)

type MonadContext :: * -> * -> * -> * -> * -> *
type MonadContext msg = KleisliContext Show (RIO (GLogFunc msg)) (StateT Vector)

--------------------------------------------------------------------------------
-- Messaging

type Rdr action = GLogFunc (Msg action)

data Msg action = AsPlayer String (PlayerMsg action) | UStart | UEnd | WithinU (Msg action) | CalledK (Msg action) | VChooseAction action
  deriving Show

data PlayerMsg action = Outputting | SamplePayoffs (SamplePayoffsMsg action) | AverageUtility (AverageUtilityMsg action)
  deriving Show

data SamplePayoffsMsg action = SampleAction action | SampleRootMsg Int (Msg action) | SampleAverageIs Double
  deriving Show

data AverageUtilityMsg action = StartingAverageUtil | AverageRootMsg (Msg action) | AverageActionChosen action | AverageIterAction Int action | AverageComplete Double
  deriving Show

--------------------------------------------------------------------------------
-- Basic types

type IOOpenGame msg a b x s y r = OpenGame (MonadOptic msg) (MonadContext msg) a b x s y r

type Agent = String

data DiagnosticsMC y = DiagnosticsMC {
  playerNameMC :: String
  , averageUtilStrategyMC :: Double
  , samplePayoffsMC :: [Double]
  , optimalMoveMC :: y
  , optimalPayoffMC :: Double
  }
  deriving (Show)

dependentDecisionIO
  :: forall x action. (Show x) => String
  -> Int
  -> [action]
  -> IOOpenGame (Msg action) '[Kleisli CondensedTableV x action] '[(RIO (Rdr action)) (DiagnosticsMC action)] x () action Double

dependentDecisionIO name sampleSize ys = OpenGame play evaluate where
  play :: List '[Kleisli CondensedTableV x action]
       -> MonadOptic (Msg action) x () action Double
  play (strat :- Nil) =
    KleisliOptic v u
    where
      v x = do
        g <- newStdGen
        gS <- newIOGenM g
        action <- genFromTable (runKleisli strat x) gS
        glog (VChooseAction action)
        return ((),action)
      u () r = modify (adjustOrAdd (+ r) r name)


  evaluate :: List '[Kleisli CondensedTableV x action]
           -> MonadContext (Msg action) x () action Double
           -> List '[(RIO (Rdr action)) (DiagnosticsMC action)]
  evaluate (strat :- Nil) (KleisliContext h k) =
    output :- Nil
    where

      output =
        RIO.mapRIO (contramap (AsPlayer name)) $ do
        glog Outputting
        zippedLs <- RIO.mapRIO (contramap SamplePayoffs) samplePayoffs
        let samplePayoffs' = map snd zippedLs
        let (optimalPlay, optimalPayoff0) = maximumBy (comparing snd) zippedLs
        (currentMove, averageUtilStrategy') <- RIO.mapRIO (contramap AverageUtility) averageUtilStrategy
        return  DiagnosticsMC{
            playerNameMC = name
          , averageUtilStrategyMC = averageUtilStrategy'
          , samplePayoffsMC = samplePayoffs'
          , optimalMoveMC = optimalPlay
          , optimalPayoffMC = optimalPayoff0
          }

        where
          -- Sample the average utility from all actions
          samplePayoffs = do vs <- mapM sampleY ys
                             pure vs
            where
              -- Sample the average utility from a single action
               sampleY :: action -> RIO (GLogFunc (SamplePayoffsMsg action)) (action, Double)
               sampleY y = do
                  glog (SampleAction y)
                  ls1 <- mapM (\i -> do v <- RIO.mapRIO (contramap (SampleRootMsg i)) $ u y
                                        pure v) [1..sampleSize]
                  let average =  (sum ls1 / fromIntegral sampleSize)
                  glog (SampleAverageIs average)
                  pure (y, average)

          -- Sample the average utility from current strategy
          averageUtilStrategy = do
            glog StartingAverageUtil
            (_,x) <- RIO.mapRIO (contramap AverageRootMsg) h
            g <- newStdGen
            gS <- newIOGenM g
            actionLS' <- mapM (\i -> do
                                        v <- RIO.mapRIO (contramap AverageRootMsg) $ action x gS
                                        glog (AverageActionChosen v)
                                        pure v)
                             [1.. sampleSize]
            utilLS  <- mapM (\(i,a) ->
                                   do glog (AverageIterAction i a)
                                      v <- RIO.mapRIO (contramap AverageRootMsg) $ u a
                                      pure v
                             )
                        (zip [1 :: Int ..] actionLS')
            let average = (sum utilLS / fromIntegral sampleSize)
            glog (AverageComplete average)
            return (x, average)
            where action x gS = do
                    genFromTable (runKleisli strat x) gS

          u y = do
             glog UStart
             (z,_) <- RIO.mapRIO (contramap WithinU) h
             v <-
              RIO.mapRIO (contramap CalledK) $
              evalStateT (do r <-  k z y
                             mp <- gets id
                             gets ((+ r) . HM.findWithDefault 0.0 name))
                          HM.empty
             glog UEnd
             pure v

-- Support functionality for constructing open games
fromLens :: (x -> y) -> (x -> r -> s) -> IOOpenGame msg '[] '[] x s y r
fromLens v u = OpenGame {
  play = \Nil -> KleisliOptic (\x -> return (x, v x)) (\x r -> return (u x r)),
  evaluate = \Nil _ -> Nil}


fromFunctions :: (x -> y) -> (r -> s) -> IOOpenGame msg '[] '[] x s y r
fromFunctions f g = fromLens f (const g)


-- discount Operation for repeated structures
discount :: String -> (Double -> Double) -> IOOpenGame msg '[] '[] () () () ()
discount name f = OpenGame {
  play = \_ -> let v () = return ((), ())
                   u () () = modify (adjustOrAdd f (f 0) name)
                 in KleisliOptic v u,
  evaluate = \_ _ -> Nil}


--------------------------------------------------------------------------------
-- Logging

logFuncSilent :: CallStack -> Msg action -> IO ()
logFuncSilent _ _ = pure ()

-- ignore this one
-- logFuncTracing :: Show action => CallStack -> Msg action -> IO ()
-- logFuncTracing _ (AsPlayer _ (SamplePayoffs (SampleRootMsg _ (CalledK {})))) = pure ()
-- logFuncTracing _ (AsPlayer _ (AverageUtility (AverageRootMsg (CalledK {})))) = pure ()
-- logFuncTracing callStack msg = do
--   case getCallStack callStack of
--      [("glog", srcloc)] -> do
--        -- This is slow - consider moving it elsewhere if speed becomes a problem.
--        fp <- makeRelativeToCurrentDirectory (srcLocFile srcloc)
--        S8.putStrLn (S8.pack (prettySrcLoc0 (srcloc{srcLocFile=fp}) ++ show msg))
--      _ -> error "Huh?"

prettySrcLoc0 :: SrcLoc -> String
prettySrcLoc0 SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, ": "
      ]

data Readr = Readr { indentRef :: IORef Int }
logFuncStructured indentRef _ msg = flip runReaderT Readr{indentRef} (go msg)

  where

   go = \case
     AsPlayer player msg -> do
       case msg of
         Outputting -> pure ()
         SamplePayoffs pmsg ->
           case pmsg of
             SampleAction action -> logln ("SampleY: " ++ take 1 (show action))
             SampleRootMsg i msg -> do
               case msg of
                 UStart -> logstr "u["
                 CalledK msg -> case msg of
                   VChooseAction action -> logstr (take 1 (show action))
                   _ -> pure ()
                 UEnd -> do logstr "]"; newline
                 _ -> pure ()
             _ -> pure ()
         _ -> pure ()
     _ -> pure ()

   logln :: String -> (ReaderT Readr IO) ()
   logln s = do newline; logstr s; newline

   logstr :: String -> (ReaderT Readr IO) ()
   logstr s = liftIO $ S8.putStr (S8.pack s)

   newline  :: ReaderT Readr IO ()
   newline =
      do Readr{indentRef} <- ask
         liftIO $
          do i <- readIORef indentRef
             S8.putStr ("\n" <> S8.replicate i ' ')


   indent :: ReaderT Readr IO ()
   indent = (do Readr{indentRef} <- ask; liftIO $ modifyIORef' indentRef (+4))

   deindent :: ReaderT Readr IO ()
   deindent =  (do Readr{indentRef} <- ask; liftIO $ modifyIORef' indentRef (subtract 4))

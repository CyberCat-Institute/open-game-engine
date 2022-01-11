module OpenGames.Engine.MonadOptic where

import           OpenGames.Engine.KleisliOptics
import           Control.Monad.State hiding (state)
import           Data.HashMap as HM hiding (null,map,mapMaybe)
import qualified RIO
import           RIO (RIO, glog, GLogFunc, HasGLogFunc(..))


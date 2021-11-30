module Engine.ProbabilityAlternative where
import           Control.Monad
import           Control.Monad.State
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Numeric.Probability.Distribution
import           System.Random.MWC.CondensedTable
import           System.Random.Stateful


data CTable a = CTable
   { ctable :: !(CondensedTableV a)
   , population :: !(V.Vector (a,Double))
   }

constructCTable :: V.Vector (a,Double) -> CTable a
constructCTable v = CTable
  { population = v
  , ctable     = tableFromProbabilities v
  }

newtype ProbT m e a = ProbT
  { runProbT :: StateT (CTable e, StdGen) m a
  }
instance Monad m => Functor (ProbT m e) where fmap = liftM
instance Monad m => Applicative (ProbT m e) where pure = return; (<*>) = ap
instance Monad m => Monad (ProbT m e) where
  return = ProbT . return
  (>>=) m f =
    ProbT
      (do (table, gen) <- get
          let (a, g) = runStateGen gen (genFromTable $ ctable table)
          -- In 'probability' they combine probabilities here. But,
          -- perhaps instead you could combine them... (see below) and
          -- then this monad instance is deleted, and just derived
          -- from State.
          undefined)

uniform :: Monad m => Vector (e, Double) -> ProbT m e e
uniform vs = do
  setUniform vs
  draw

-- ... Perhaps you could combine them here?
setUniform :: Monad m => Vector (e, Double) -> ProbT m e ()
setUniform vs = ProbT (modify (\(s,g) -> (constructCTable vs,g)))

draw :: Monad m => ProbT m e e
draw =
  ProbT
    (do (table, gen) <- get
        let (e, g) = runStateGen gen (genFromTable $ ctable table)
        pure e)

-- Finally, you need a `decons', such as,
-- The decons is only used for the support -- could be simplified with `population' operator saved in that way
decons :: ProbT m e () -> m [(e,Double)]
decons = undefined
-- ... and perhaps an internal CTable would do the trick for this. But
-- what is the project doing with the resulting list? Good question to
-- ask.
--
--


-- Need `expected'
expected :: Num a => ProbT m e a -> a
expected = undefined

-- But that should be doable via the table

-- Need `mapMaybe`
mapMaybeProb :: (Monad m, Eq y) => y -> Vector ((x,y), Double)  -> ProbT m x ()
mapMaybeProb y v  =
  let v' = V.mapMaybe (\((x, y'),val) -> if y' == y then Just (x,val) else Nothing) v
      in ProbT (modify (\(s,g) -> (constructCTable  v',g)))

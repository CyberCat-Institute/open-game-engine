import Data.ByteString.Lazy.Char8 (pack)
import qualified Examples.HEVM as HEVM
import qualified Examples.Prisoner as P
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main =
  defaultMain $
    testGroup
      "HEVM tests"
      [ goldenVsStringDiff
          "Should detect deviation when better transaction is available"
          (\ref new -> ["git", "diff", "--no-index", ref, new])
          "golden/hevm.golden"
          (pack <$> HEVM.outcomeAutomatic),
        goldenVsStringDiff
          "Should detect deviation for the prisoner's dilemma"
          (\ref new -> ["git", "diff", "--no-index", ref, new])
          "golden/prisoner.golden"
          (pack <$> P.outcomeAutomatic)
      ]

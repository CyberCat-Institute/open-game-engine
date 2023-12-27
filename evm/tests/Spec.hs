
import Examples.HEVM
import Test.Tasty
import Test.Tasty.Golden
import Data.ByteString.Lazy.Char8 (pack)

main :: IO ()
main = defaultMain $
  testGroup "HEVM tests" [
    goldenVsStringDiff
        "Should detect deviation when better transaction is available"
        (\ref new -> ["git", "diff", "--no-index", ref, new])
        "golden/hevm.golden"
        (pack <$> outcomeAutomatic)
    ]


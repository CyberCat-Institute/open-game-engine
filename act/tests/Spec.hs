import Data.ByteString.Lazy.Char8 (pack)
import Examples.Player
import OpenGames.Engine.Diagnostics (generateOutputStr)
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main =
  defaultMain $
    testGroup
      "ACT tests"
      [ goldenVsStringDiff
          "Should find the best transaction"
          (\ref new -> ["git", "diff", "--no-index", ref, new])
          "golden/foo.golden"
          (pure $ pack $ generateOutputStr foo),
        goldenVsStringDiff
          "Should find the best swaps between two AMMs"
          (\ref new -> ["git", "diff", "--no-index", ref, new])
          "golden/ev.golden"
          (pure $ pack $ generateOutputStr ev),
        goldenVsStringDiff
          "Should find the optimal transaction order"
          (\ref new -> ["git", "diff", "--no-index", ref, new])
          "golden/ev1.golden"
          (pure $ pack $ generateOutputStr ev1)
      ]

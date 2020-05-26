module OpenGames.Examples.Governance.ChoosingRandomly where

import Numeric.Probability.Distribution
import OpenGames.Preprocessor.AbstractSyntax
import OpenGames.Engine.OpenGamesClass
import OpenGames.Engine.BayesianDiagnostics

majority :: (Fractional prob) => Either () () -> Either () () -> T prob (Either () ())
majority (Left ()) (Left ()) = certainly (Left ())
majority (Right ()) (Right ()) = certainly (Right ())
majority _ _ = uniform [Left (), Right ()]

-- Next time: 2 players choose between PD + stag hunt, tie broken randomly

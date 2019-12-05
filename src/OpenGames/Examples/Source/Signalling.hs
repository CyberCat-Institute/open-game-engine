module OpenGames.Examples.Source.Signalling where

import OpenGames.Preprocessor.AbstractSyntax

-- Spence's signalling game

signalling = Block [] []
                   [Line [] [] "nature (fromFreqs [(LowProductivity, 8), (HighProductivity, 1)])" ["productivity"] [],
                    Line ["productivity"] [] "decision \"worker-effort\" [LowEffort, HighEffort]" ["effort"] ["signallingUtilityWorker productivity effort wage contract"],
                    Line ["effort"] [] "decision \"firm\" [LowWage, HighWage]" ["wage"] ["signallingUtilityFirm productivity wage contract"],
                    Line ["productivity", "wage"] [] "decision \"worker-contract\" [Accept, NotAccept]" ["contract"] ["signallingUtilityWorker productivity effort wage contract"]]
                   [] []


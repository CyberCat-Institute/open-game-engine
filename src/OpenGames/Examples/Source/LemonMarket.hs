module OpenGames.Examples.Source.LemonMarket where

import OpenGames.Preprocessor.AbstractSyntax

lemonMarket = Block [] []
                    [Line [] [] "nature (fromFreqs [(Good, 1), (Bad, 4)])" ["quality"] [],
                     Line ["quality"] [] "decision \"seller\" [Low, High]" ["price"] ["lemonUtilitySeller quality price buy"],
                     Line ["price"] [] "decision \"buyer\" [Buy, NotBuy]" ["buy"] ["lemonUtilityBuyer quality price buy"]]
                    [] []


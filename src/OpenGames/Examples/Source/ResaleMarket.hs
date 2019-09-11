module OpenGames.Examples.Source.ResaleMarket where

import OpenGames.Preprocessor.AbstractSyntax

-- Public good resale market

resaleMarket = Block [] []
                     [Line [] [] "nature pgResale_prior" ["sellerType", "buyerType"] [],
                      Line ["sellerType"] [] "decision \"seller\" [LowPrice, MediumPrice, HighPrice]" ["price"] ["sellerUtility sellerType price buy"],
                      Line ["price"] [] "decision \"buyer\" [BuyGood, NotBuyGood]" ["buy"] ["buyerUtility buyerType price buy"]]
                     [] []


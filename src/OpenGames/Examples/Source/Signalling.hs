module OpenGames.Examples.Source.Signalling where

import OpenGames.Preprocessor.AbstractSyntax

-- Spence's signalling game

signalling = Block [] []
                   [Line [] [] "nature(fromFreqs [(LowProductivity, 8), (HighProductivity, 1)])" ["productivity"] [],
                    Line ["productivity"] [] "decision \"worker-effort\" [LowEffort, HighEffort]" ["effort"] ["signallingUtilityWorker productivity effort wage contract"],
                    Line ["effort"] [] "decision \"firm\" [LowWage, HighWage]" ["wage"] ["signallingUtilityFirm productivity wage contract"],
                    Line ["productivity", "wage"] [] "decision \"worker-contract\" [Accept, NotAccept]" ["contract"] ["signallingUtilityWorker productivity effort wage contract"]]
                   [] []

-- Beer-Quiche game

beerquiche = Block [] []
                   [Line [] [] "nature (fromFreqs [(Weak, 3), (Strong, 7)])" ["strength"] [],
                    Line ["strength"] [] "decision \"food-choice\" [Beer, Quiche]" ["food"] ["signallingUtilitySender strength food fight"],
                    Line ["food"] [] "decision \"receiver\" [Duel, NoDuel]" ["fight"] ["signallingUtilityReceiver strength fight"]]
                   [] []

-- Simpler version of the education game

education = Block [] []
                  [Line [] [] "nature (fromFreqs [(Clever, 3), (Normal, 7)])" ["mind"] [],
                   Line ["mind"] [] "decision \"edu-choice\" [Degree, NoDegree]" ["edu"] ["signallingUtilityApplicant mind edu dec"],
                   Line ["edu"] [] "decision \"company\" [Hire, NoHire]" ["dec"] ["signallingUtilityCompany mind dec"]]
                  [] []

-- Looking for a roommate

roommate = Block [] []
                  [Line [] [] "nature (fromFreqs [(Funny, 3), (Serious, 2)])" ["mood"] [],
                   Line ["mood"] [] "decision \"clown-choice\" [On, Off]" ["clown"] ["signallingUtilityRoommate mood clown casting"],
                   Line ["clown"] [] "decision \"coop\" [Offer, Deny]" ["casting"] ["signallingUtilityCoop mood casting"]]
                  [] []

module OpenGames.Examples.Consensus.Censor where

data CensorMove = Honest | Censor deriving (Eq, Ord, Show)
data CarolObservation = Online | Offline deriving (Eq, Ord, Show)
type Deposit = Double

carolObservation :: (CensorMove, CensorMove, CensorMove) -> (CarolObservation, CarolObservation, CarolObservation)
carolObservation (Honest, Honest, _) = (Online, Online, Online)
carolObservation (Honest, _, Honest) = (Online, Online, Online)
carolObservation (_, Honest, Honest) = (Online, Online, Online)
carolObservation (x, y, z) = (f x, f y, f z)
  where f Honest = Offline
        f Censor = Online
{--
payoff :: Deposit -> CarolObservation -> Double
payoff Online = 1.0
payoff Offline = 0.0

depositGameSrc = Block ["costOfCapital"] []
  [Line ["\"Alice\"", "()"] [] "roleDecision [0.0 .. 10.0]" ["aliceStake"] ["-costOfCapital*aliceStake"],
   Line ["\"Bob\"", "()"] [] "roleDecision [0.0 .. 10.0]" ["bobStake"] ["-costOfCapital*bobStake"]]
  ["aliceStake", "bobStake"] []
-}

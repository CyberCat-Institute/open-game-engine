module OpenGames.Engine.Diagnostics where

-- Open games in this implementation are paramterised over a monoid of return values of the equilibrium function
-- Here we introduce a datatype of diagnostic information that records useful information about failures of equilibirum
-- (the monoid instance to be used is lists of this)

data DiagnosticInfo = DiagnosticInfo {
  player            :: String,
  state             :: String,
  unobservableState :: String,
  strategy          :: String,
  payoff            :: String,
  optimalMove       :: String,
  optimalPayoff     :: String}
  deriving (Show)

-- Apparently Bool doesn't have an existing Monoid instance. Luckily the conjunctive one is the more natural one
-- This file is also a reasonable place to put this instance

instance Semigroup Bool where (<>) = (&&)
instance Monoid Bool where mempty  = True

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import OpenGames.Examples.Consensus.AndGateMarkov

main = print $ andGateMarkovEq 4

{-# LANGUAGE TemplateHaskell #-}
module Example where

import Act.TH
import Act.Prelude


$(act2OG "amm.act")

-- extractSwap1 :: [AbiType] -> Int
-- extractSwap1 [AbiUIntType amt] = amt
-- extractSwap1 _ = error "unexpected type, please report this error"
--
-- extractSwap2 :: [AbiType] -> Int
-- extractSwap2 [AbiUIntType amt] = amt
-- extractSwap2 _ = error "unexpected type, please report this error"
--
-- swapWithAmount :: AmmState -> Transaction  -> (AmmState)
-- swapWithAmount (AmmState reserve0 reserve1) transaction  =
--   case method transaction of
--     "swap0" ->
--         -- derived from implementation
--         let (amt) = extractSwap1 (arguments transaction) in
--         AmmState
--         (reserve0 + amt)
--         ((reserve0 * reserve1) `div` (reserve0 + amt) + 1)
--     "swap1" ->
--         let (amt) = extractSwap2 (arguments transaction) in
--         AmmState
--         ((reserve0 * reserve1) `div` (reserve1 + amt) + 1)
--         (reserve1 + amt)
--     _ -> error ("unexpected method: " ++ method transaction)
--

module Coin (Coins, makeCoins, modifyCoins) where

type Coins = Int

makeCoins :: Int -> Coins
makeCoins = max 0

modifyCoins :: Int -> Coins -> Coins
modifyCoins x y = makeCoins (x + y)



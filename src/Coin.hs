module Coin (Coins, makeCoins, modifyCoins, startingCoin) where

type Coins = Int

startingCoin :: Int
startingCoin = 3

makeCoins :: Int -> Coins
makeCoins = max 0

modifyCoins :: Int -> Coins -> Coins
modifyCoins x y = makeCoins (x + y)



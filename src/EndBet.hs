module EndBet (betEnd, EndBet, EndStack, EndStatus(EndWin, EndLose), redeem) where

import Data.Map hiding (map, partition)
import Data.List (partition)
import Ranking (Ranking)
import qualified Ranking as Rk
import Color
import Utility

data EndBet = EndBet 
    { playerIdBet :: PlayerId 
    , colorBet :: Color
    }

data EndStack = EndWinLose [EndBet] [EndBet]
newEndStack = EndWinLose [] []

data EndStatus = EndWin | EndLose

betEnd :: PlayerId -> Color -> EndStatus -> EndStack -> EndStack
betEnd pid color status (EndWinLose win lose)
  = case status of
      EndWin  -> EndWinLose (EndBet pid color : win) lose
      EndLose -> EndWinLose win (EndBet pid color : lose)

profit :: [Int]
profit = [8, 5, 3, 2] ++ repeat 1

loss :: [Int]
loss = repeat (-1)

redeemProfit :: [PlayerId] -> [Int] -> Map PlayerId Int
redeemProfit players profit = fromListWith (+) (zip players profit)

redeemWithColor :: Color -> [EndBet] -> Map PlayerId Int
redeemWithColor color bets =
    let (correct, incorrect) = partition (\b -> colorBet b == color) (reverse bets)
        winMap = redeemProfit (map playerIdBet correct) profit
        loseMap = redeemProfit (map playerIdBet incorrect) loss
     in unionWith (+) winMap loseMap

redeem :: Ranking Color -> EndStack -> [(PlayerId, Int)]
redeem rank (EndWinLose winStack loseStack) =
    let predictWin = redeemWithColor (Rk.getFirst rank) winStack
        predictLose = redeemWithColor (Rk.getLast rank) loseStack
     in toList $ unionWith (+) predictWin predictLose
        

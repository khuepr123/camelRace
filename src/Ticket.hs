module Ticket (Ticket, TicketHall, newTicketHall, takeTicket, redeem) where

import Coin
import Ranking (Ranking)
import qualified Ranking as Rk
import Color
import Utility
import Data.Array

data Ticket = RankedFirst Color Int
rankedSecond = 1
rankedNone   = -1

type TicketHall = Array Color [Int]

startingStack = [5, 3, 2, 2]

newTicketHall :: TicketHall
newTicketHall = array rangeDef $ map (, startingStack) allValues

takeTicket :: TicketHall -> Color -> Maybe (Ticket, TicketHall)
takeTicket hall color =
    case hall ! color of
      [] -> Nothing
      worth : stack -> Just (RankedFirst color worth, hall // [(color, stack)])

redeemSingle :: Ranking Color -> Ticket -> Int
redeemSingle rank (RankedFirst color rankedFirst)
  | color == Rk.getFirst rank  = rankedFirst
  | color == Rk.getSecond rank = rankedSecond
  | otherwise                  = rankedNone

redeem :: Ranking Color -> [Ticket] -> Int
redeem rank = sum . map (redeemSingle rank)

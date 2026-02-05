module Ticket (Ticket, TicketHall, newTicketHall, takeTicket, redeem) where

import Coin
import Ranking (Ranking)
import qualified Ranking as Rk
import Color
import Utility
import Data.Array
import Control.Monad.State

data Ticket = RankedFirst Color Int
rankedSecond = 1
rankedNone   = -1

type TicketHall = Array Color [Int]

startingStack = [5, 3, 2, 2]

newTicketHall :: TicketHall
newTicketHall = array rangeDef $ map (, startingStack) allValues

-- takeTicket :: TicketHall -> Color -> Maybe (Ticket, TicketHall)
-- takeTicket hall color =
--     case hall ! color of
--       [] -> Nothing
--       worth : stack -> Just (RankedFirst color worth, hall // [(color, stack)])

-- takeTicket :: Color -> TicketHall -> (Either String Ticket, TicketHall)
takeTicket :: Color -> TicketHall -> Either String (Ticket, TicketHall)
takeTicket color = runStateT $ do
    hall <- get
    case hall ! color of
      [] -> lift $ Left "No more ticket left for this camel"
      worth : stack -> do
          put $ hall // [(color, stack)]
          return (RankedFirst color worth)

redeemSingle :: Ranking Color -> Ticket -> Int
redeemSingle rank (RankedFirst color rankedFirst)
  | color == Rk.getFirst rank  = rankedFirst
  | color == Rk.getSecond rank = rankedSecond
  | otherwise                  = rankedNone

redeem :: Ranking Color -> [Ticket] -> Int
redeem rank = sum . map (redeemSingle rank)

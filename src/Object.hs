module Object () where

import qualified Data.Map as Mp
import Data.Map (Map, (!))

newtype Wealth = Int

makeWealth :: Int -> Wealth
makeWealth = id

data Ticket = RankedFirst Int
rankedSecond = 1
rankedNone   = -1

newtype TicketHall = Map Color [Ticket]

newTicketHall :: TicketHall
newTicketHall = Mp.fromList $ map (, newStoreStack) allColor
    where newStoreStack = map RankedFirst [5, 3, 2, 2]

takeTicket :: TicketHall -> Color -> Maybe (Ticket, TicketHall)
takeTicket hall color =
    case hall ! color of
      [] -> Nothing
      ticket : stack -> Just (ticket, Mp.insert color stack hall)



data FinishCard = Finish Color

data SpectatorTile = Push | Pull

data Action = BetWinOverall | BetLoseOverall | BuyTicket | RollDice | PlaceSpectator

data Player = Player Wealth [Ticket]

data GameState = GameState 
    { track :: RaceTrack 
    , hall :: TicketHall 
    , players :: 
    


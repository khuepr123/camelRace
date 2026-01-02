module CamelRace () where


import Player
import RaceTrack
import RandomTrack
import Ranking (Ranking)
import qualified Ranking as Rk
import Color
import Utility
import System.Random
import Data.List

data Action = BetWinOverall | BetLoseOverall | BuyTicket Color | RollDice | PlaceSpectator 

instance Show Action where
    show BetWinOverall = "Bet the overall winning camel"
    show BetLoseOverall = "Bet the overall losing camel"
    show BuyTicket = "Choose a camel to gamble on"
    show RollDice = "Roll the dice"
    show PlaceSpectator = "Place a spectator tile"

data GameState = GameState 
    { ticketHall :: TicketHall
    , players :: Array PlayerId Player
    , endBets :: EndBets
    , randomTrack :: RandomTrack
    , seed :: StdGen
    }

play :: Action -> GameState -> Either String GameState
play action 

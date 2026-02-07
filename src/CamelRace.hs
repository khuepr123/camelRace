{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module CamelRace where

import EndBet
import Player
import RaceTrack
import RandomTrack
import Ticket
-- import Ranking (Ranking)
-- import qualified Ranking as Rk
import Color
import Utility

import Control.Monad.State

import System.Random

import Control.Lens

data Action = BetWinOverall Color | BetLoseOverall Color | BuyTicket Color | RollDice | PlaceSpectator Tile Spectator

-- instance Show Action where
--     show BetWinOverall = "Bet the overall winning camel"
--     show BetLoseOverall = "Bet the overall losing camel"
--     show BuyTicket = "Choose a camel to gamble on"
--     show RollDice = "Roll the dice"
--     show PlaceSpectator = "Place a spectator tile"

data Event = EndBetted EndStatus | MovedCamel Camel 
  | NoEvent | PlacedSpectator Tile | BoughtTicket Color

data GameState = GameState
    { _ticketHall :: TicketHall
    , _players :: PlayerBase
    , _endStack :: EndStack
    , _randomTrack :: RandomTrack
    , _seed :: StdGen
    , _events :: [Event]
    }

$(makeLenses ''GameState)

newGameState :: [Name] -> StdGen -> GameState
newGameState names rngSeed =
    GameState { _ticketHall  = newTicketHall
              , _players     = newPlayerBase names
              , _endStack    = newEndStack
              , _randomTrack = createdTrack
              , _seed        = newSeed
              , _events      = []
              }
                  where (createdTrack, newSeed) = runState genTrack rngSeed

getNextPlayer :: MonadState GameState m => PlayerId -> m PlayerId
getNextPlayer pID = gets (((pID + 1) `mod`) . playerCount . view players)


gameEnded :: MonadState GameState m => m GameStatus
gameEnded = gets (isEnded . fst . view randomTrack)

-- tryBetEnd :: PlayerId -> Color -> EndStatus -> (PlayerBase, EndStack) -> Either String (PlayerBase, EndStack)
-- takeTicket :: Color -> TicketHall -> Either String (Ticket, TicketHall)
-- getTicket :: Ticket -> PlayerId -> PlayerBase -> PlayerBase
-- roll :: RandomTrack -> Either RaceTrack RandomTrack
-- restock :: RaceTrack -> State StdGen RandomTrack
-- updatePlayer :: PlayerId -> (Player -> Player) -> PlayerBase -> PlayerBase
-- changeSpectator :: Tile -> Spectator -> PlayerId -> RaceTrack -> Either String RaceTrack

data RoundStatus = Ended | Ongoing

instance Show Event where
    show (EndBetted stat) = ""
    show (MovedCamel camel) = "the " ++ showCamel camel ++ " camel moved"
        where showCamel (Normal color) = show color
              showCamel (Crazy color) = show color
    show NoEvent = ""
    show (PlacedSpectator tile) = "A new spectator tile was placed on tile " ++ show tile
    show (BoughtTicket color) = "" 
    
    

-- set is just over with const

-- withRNG :: State StdGen a -> StateT GameState (Either String) a
withRNG :: MonadState GameState m => State StdGen b -> m b
withRNG gen = do
    thisRNG <- gets (view seed)
    let (result, newRNG) = runState gen thisRNG
    modify $ set seed newRNG
    return result


cleanupRound :: MonadState GameState m => RaceTrack -> m ()
cleanupRound raceTrack = do
    modify $ over players (cashOut ranking)
    -- TODO: clean all spectator
    --
    restocked <- withRNG . restock . clearSpectator $ raceTrack
    modify $ set randomTrack restocked

    modify $ set ticketHall newTicketHall
        where ranking = getRanking raceTrack


cleanupGame :: MonadState GameState m => m PlayerBase
cleanupGame = do
    raceTrack <- gets (fst . view randomTrack)
    cleanupRound raceTrack

    playerBase :: PlayerBase <- gets (view players)
    lastBets :: EndStack <- gets (view endStack)
    return $ cashAll (getRanking raceTrack) lastBets playerBase






coinPerRoll :: Int
coinPerRoll = 1



play :: PlayerId -> Action -> StateT GameState (Either String) RoundStatus
play pID action = do
    thisState <- get
    (roundStatus, event) <- case action of
      BetWinOverall color -> betEndWith color EndWin thisState
      BetLoseOverall color -> betEndWith color EndLose thisState
      BuyTicket color ->
          case takeTicket color (view ticketHall thisState) of
            Right (ticket, updatedTicketHall) -> do
                modify $ set ticketHall updatedTicketHall
                modify $ over players (getTicket ticket pID)
                return (Ongoing, BoughtTicket color)
            Left err -> lift $ Left err
      RollDice -> do
                    addToPlayer pID
                    updateMayPID mayPID 
                    (,MovedCamel camel) <$> case resultTrack of
                        Right newRandomTrack -> do
                            modify $ set randomTrack newRandomTrack
                            return Ongoing
                        Left raceTrack -> do
                            cleanupRound raceTrack
                            return Ended
                    where (mayPID, resultTrack, camel) = roll (view randomTrack thisState) 
                          updateMayPID = \case
                            Nothing -> pure ()
                            Just tilePID -> addToPlayer tilePID
                          addToPlayer targetID = modify $ over players (updatePlayer targetID (earn coinPerRoll))
      PlaceSpectator tile spectator -> case changeSpectator tile spectator pID track of
                                         Right createdTrack -> do
                                             modify $ set randomTrack (createdTrack, moves)
                                             return (Ongoing, PlacedSpectator tile)
                                         Left err -> lift $ Left err
          where (track, moves) = view randomTrack thisState
    modify $ over events (event:)
    return roundStatus
    where betEndWith color endStatus thisState =
              case tryBetEnd pID color endStatus (view players thisState, view endStack thisState) of
                Right (newPlayers, createdEndStack) -> do
                    modify $ set players newPlayers
                    modify $ set endStack createdEndStack
                    return (Ongoing, EndBetted endStatus)
                Left err -> lift $ Left err
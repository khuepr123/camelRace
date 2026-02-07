module Player where

import EndBet
import Ticket
import Coin
import Ranking (Ranking)
import Color
import Utility
import Data.List (delete)
import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))

type PlayerBase = Vector Player
type Name = String

newPlayerBase :: [Name] -> PlayerBase
newPlayerBase = V.fromList . map createPlayer
    where createPlayer newName = Player { name = newName
                                        , wealth = startingCoin
                                        , tickets = []
                                        , remainingEndBet = allValues
                                        }

data Player = Player
    { name :: Name
    , wealth :: Coins
    , tickets :: [Ticket]
    , remainingEndBet :: [Color]
    }

instance Show Player where
    show player = name player ++ ": " ++ show (wealth player) ++ " Coins"

getName :: PlayerId -> PlayerBase -> Name
getName pID = name . (! pID)

findAndRemove :: Eq a => a -> [a] -> Maybe [a]
findAndRemove x l
  | x `elem` l = Just (delete x l)
  | otherwise = Nothing

updatePlayer :: PlayerId -> (Player -> Player) -> (PlayerBase -> PlayerBase)
updatePlayer pID func = fst . updatePlayerWith pID ((,()) . func)

updatePlayerWith :: PlayerId -> (Player -> (Player, a)) -> PlayerBase -> (PlayerBase, a)
updatePlayerWith pID func playerBase =
    let (new, effect) = func $ playerBase ! pID
     in (playerBase // [(pID, new)], effect)

getTicket :: Ticket -> PlayerId -> PlayerBase -> PlayerBase
getTicket ticket pID =
    updatePlayer pID addTicket 
    where addTicket player = player {tickets = ticket : tickets player}
    
tryBetEnd :: PlayerId -> Color -> EndStatus -> (PlayerBase, EndStack) -> Either String (PlayerBase, EndStack)
tryBetEnd pID color stat (playerBase, bets) =
    case findAndRemove color (remainingEndBet $ playerBase ! pID) of
      Nothing -> Left "User already bet this color"
      Just l  -> Right (changedBase, newBets)
          where transform player = player {remainingEndBet = l}
                newBets = betEnd pID color stat bets
                changedBase = updatePlayer pID transform playerBase

earn :: Int -> Player -> Player
earn earning player = player { wealth = modifyCoins (wealth player) earning }

earnPID :: PlayerId -> Int -> PlayerBase -> PlayerBase
earnPID pID coin = updatePlayer pID (earn coin)

cashOutIndividual :: Ranking Color -> Player -> Player
cashOutIndividual ranking player = 
    (earn earning player)
        { tickets = [] }
            where earning = Ticket.redeem ranking (tickets player)

cashOut :: Ranking Color -> PlayerBase -> PlayerBase
cashOut ranking = V.map (cashOutIndividual ranking)

cashAll :: Ranking Color -> EndStack -> PlayerBase -> PlayerBase
cashAll ranking endStack playerBase =
    V.accum addMoney (cashOut ranking playerBase) (EndBet.redeem ranking endStack)
        where addMoney = flip earn

playerCount :: PlayerBase -> Int
playerCount = V.length

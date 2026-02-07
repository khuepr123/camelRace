module RaceTrack (Camel (Normal, Crazy), CrazyColor, allCamels, RaceTrack, MoveUnit, Spectator(Push, Pull), newTrack, updateTrack, getRanking, changeSpectator, deleteSpectator, ongoing, Tile, trackRange, TileStatus(..), trackToList, exampleTrack, clearSpectator, isEnded, GameStatus(..)) where

import Data.Ix (inRange, range)
import Ranking (Ranking)
import qualified Ranking as Rk
import Color
import Utility
import qualified Data.Map as Mp
import Data.Map (Map)
import Data.Maybe (mapMaybe)

trackRange :: (Tile, Tile)
trackRange = (1, 16)

data Camel = Normal Color | Crazy CrazyColor
    deriving (Eq, Ord, Show)

allCamels :: [Camel]
allCamels = map Normal (allValues :: [Color]) ++ map Crazy (allValues :: [CrazyColor])

type Tile = Int

data Spectator = Push | Pull

fromSpectator :: Spectator -> Int
fromSpectator Push = 1
fromSpectator Pull = -1

data TileStatus = HasCamel [Camel] | HasSpectator Spectator PlayerId
data GameStatus = Progressing | Concluded

type RaceTrack = Map Tile TileStatus

trackToList :: RaceTrack -> [Maybe TileStatus]
trackToList track = [preStart] ++ midTrack ++ [afterFinish]
    where preStart = snd <$> Mp.lookupLT (fst trackRange) track
          midTrack = map (`Mp.lookup` track) (range trackRange)
          afterFinish = snd <$> Mp.lookupGT (snd trackRange) track

addCamels :: (Tile, [Camel]) -> RaceTrack -> (Maybe PlayerId, RaceTrack)
addCamels (tile, camels) track = 
    case Mp.lookup tile track of
      Just (HasSpectator spec pID) -> (Just pID, snd $ addCamels (tile + fromSpectator spec, camels) track)
      Just (HasCamel stack) -> (Nothing, addedWith stack)
      Nothing -> (Nothing, addedWith [])
    where addedWith stack = Mp.insert tile (HasCamel (stack ++ camels)) track

hasCamel :: Camel -> TileStatus -> Maybe [Camel]
hasCamel _ (HasSpectator _ _) = Nothing
hasCamel camel (HasCamel camels)
  | camel `elem` camels = Just camels
  | otherwise           = Nothing

retrieveCamel :: Camel -> RaceTrack -> (Tile, [Camel], RaceTrack)
retrieveCamel camel track = 
    case Mp.toList $ Mp.mapMaybe (hasCamel camel) track of
      [(tile, camels)] -> let (leftover, carried) = span (/=camel) camels
                              updatedTrack = Mp.insert tile (HasCamel leftover) track
                           in (tile, carried, updatedTrack)

      _ -> error "invalid track: possibly none or more than one camel found"

type MoveUnit = (Camel, Int)

go :: MoveUnit -> Tile -> Tile
go (camel, jumpDist) tile =
    case camel of
      Normal _ -> tile + jumpDist
      Crazy _  -> tile - jumpDist

startingPosition :: Camel -> Tile
startingPosition (Normal _) = fst trackRange - 1
startingPosition (Crazy _) = snd trackRange + 1

newTrack :: [MoveUnit] -> RaceTrack
newTrack =
    foldl insertToTrack Mp.empty
        where insertToTrack :: RaceTrack -> MoveUnit -> RaceTrack
              insertToTrack track moveUnit@(camel, _) =
                    snd $ addCamels (go moveUnit (startingPosition camel), [camel]) track


updateTrack :: MoveUnit -> RaceTrack -> (Maybe PlayerId, RaceTrack)
updateTrack moveUnit@(camel, _) track =
    let (tile, camels, pendingTrack) = retrieveCamel camel track
     in addCamels (go moveUnit tile, camels) pendingTrack

hasSpectator :: Tile -> RaceTrack -> Bool
hasSpectator tile track = 
    case Mp.lookup tile track of
      Just (HasSpectator _ _) -> True
      _ -> False

changeSpectator :: Tile -> Spectator -> PlayerId -> RaceTrack -> Either String RaceTrack
changeSpectator tile spectator pID oldTrack
  | tile == fst trackRange = Left "Spectator are not allowed on first space"
  | Mp.member tile track = Left "Space occupied"
  | hasSpectator (tile - 1) track = Left "Previous space occupied"
  | hasSpectator (tile + 1) track = Left "Subsequent space occupied"
  | not $ inRange trackRange tile = Left "Tile out of bound"
  | otherwise = Right $ Mp.insert tile (HasSpectator spectator pID) track
  where track = deleteSpectator pID oldTrack

deleteSpectator :: PlayerId -> RaceTrack -> RaceTrack
deleteSpectator pID = Mp.mapMaybe killSpec
    where killSpec (HasSpectator _ x) | x == pID = Nothing
          killSpec leftover = Just leftover

getRanking :: RaceTrack -> Ranking Color
getRanking = Rk.fromList . reverse . foldMap getOnlyColor
    where getOnlyColor (HasSpectator _ _) = []
          getOnlyColor (HasCamel camels) = mapMaybe selectGoodColor camels
          selectGoodColor (Crazy _) = Nothing
          selectGoodColor (Normal c) = Just c

ongoing :: RaceTrack -> Bool
ongoing = all (inRange trackRange) . Mp.keys

clearSpectator :: RaceTrack -> RaceTrack
clearSpectator = Mp.filter (not . isSpectator)
    where isSpectator :: TileStatus -> Bool
          isSpectator (HasSpectator _ _) = True
          isSpectator (HasCamel     _  ) = False

isEnded :: RaceTrack -> GameStatus
isEnded track = if all (inRange trackRange) . Mp.keys $ track
                   then Progressing
                   else Concluded
                   


exampleTrack :: RaceTrack
exampleTrack = Mp.fromList [ (1, HasCamel [Normal Blue])
                           , (-4, HasCamel [Normal Green ,Crazy Black, Normal Red])
                           , (19, HasCamel [Normal Purple, Crazy White])
                           , (13, HasCamel [Normal Yellow])
                           , (7, HasSpectator Push 0)
                           , (10, HasSpectator Pull 1)
                           ]

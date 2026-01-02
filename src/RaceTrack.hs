module RaceTrack (Camel (Normal, Crazy), CrazyColor, allCamels, RaceTrack, MoveUnit, Spectator, newTrack, updateTrack, getRanking, addSpectator, deleteSpectator, ongoing) where

import Data.Ix (inRange)
import Ranking (Ranking)
import qualified Ranking as Rk
import Color
import Utility
import qualified Data.Map as Mp
import Data.Map ((!), Map)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe)

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

data TileStatus = HasCamel [Camel] | HasSpectator Spectator

type RaceTrack = Map Tile TileStatus

addCamels :: (Tile, [Camel]) -> RaceTrack -> RaceTrack
addCamels (tile, camels) track = 
    case Mp.lookup tile track of
      Just (HasSpectator spec) -> addCamels (tile + fromSpectator spec, camels) track
      Just (HasCamel stack) -> addedWith stack
      Nothing -> addedWith []
    where addedWith stack = Mp.insert tile (HasCamel (stack ++ camels)) track

hasCamel :: Camel -> TileStatus -> Maybe [Camel]
hasCamel _ (HasSpectator _) = Nothing
hasCamel camel (HasCamel camels)
  | camel `elem` camels = Just camels
  | otherwise           = Nothing

retrieveCamel :: Camel -> RaceTrack -> (Tile, [Camel], RaceTrack)
retrieveCamel camel track = 
    case Mp.toList $ Mp.mapMaybe (hasCamel camel) track of
      [(tile, camels)] -> let (leftover, carried) = span (/=camel) camels
                              newTrack = Mp.insert tile (HasCamel leftover) track
                           in (tile, carried, newTrack)

      _ -> error "invalid track: possibly none or more than one camel found"

type MoveUnit = (Camel, Int)

go :: MoveUnit -> Tile -> Tile
go (camel, jumpDist) tile =
    case camel of
      Normal color -> tile + jumpDist
      Crazy color -> tile - jumpDist

startingPosition :: Camel -> Tile
startingPosition (Normal _) = fst trackRange - 1
startingPosition (Crazy _) = snd trackRange + 1

newTrack :: [MoveUnit] -> RaceTrack
newTrack =
    foldl insertToTrack Mp.empty
        where insertToTrack :: RaceTrack -> MoveUnit -> RaceTrack
              insertToTrack track moveUnit@(camel, _) =
                    addCamels (go moveUnit (startingPosition camel), [camel]) track


updateTrack :: MoveUnit -> RaceTrack -> RaceTrack
updateTrack moveUnit@(camel, jumpDist) track =
    let (tile, camels, pendingTrack) = retrieveCamel camel track
     in addCamels (go moveUnit tile, camels) pendingTrack

hasSpectator :: Tile -> RaceTrack -> Bool
hasSpectator tile track = 
    case Mp.lookup tile track of
      Just (HasSpectator spec) -> True
      _ -> False

addSpectator :: Tile -> Spectator -> RaceTrack -> Either String RaceTrack
addSpectator tile spectator track
  | Mp.member tile track = Left "Space occupied"
  | hasSpectator (tile - 1) track = Left "Previous space occupied"
  | hasSpectator (tile + 1) track = Left "Subsequent space occupied"
  | not $ inRange trackRange tile = Left "Tile out of bound"
  | otherwise = Right $ Mp.insert tile (HasSpectator spectator) track

deleteSpectator :: Tile -> RaceTrack -> Either String RaceTrack
deleteSpectator tile track =
    case Mp.lookup tile track of
      Just (HasSpectator _) -> Right (Mp.delete tile track)
      Just (HasCamel _)     -> Left "Camel(s) is here"
      Nothing               -> Left "Nothing here"

getRanking :: RaceTrack -> Ranking Color
getRanking = Rk.fromList . reverse . foldMap getOnlyColor
    where getOnlyColor (HasSpectator _) = []
          getOnlyColor (HasCamel camels) = mapMaybe selectGoodColor camels
          selectGoodColor (Crazy _) = Nothing
          selectGoodColor (Normal c) = Just c

ongoing :: RaceTrack -> Bool
ongoing = all (inRange trackRange) . Mp.keys

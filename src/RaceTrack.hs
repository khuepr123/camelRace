module RaceTrack () where

import Color
import Utility
import qualified Data.Map as M
import Data.Map ((!), Map)

trackLength = 16

data Camel = Normal Color | Crazy CrazyColor
    deriving (Ord, Show, Enum, Bounded)


data CamelStatus = OnTile Int | OnCamel Camel

type RaceTrack = Map Camel CamelStatus

data TrackStatus = Ended | Ongoing

newTrack :: RaceTrack
newTrack = M.fromList $ map (,OnTile 0) (allValues Camel)


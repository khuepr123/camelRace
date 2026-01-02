module Color (Color, CrazyColor) where

import Data.Ix

data Color = Red | Green | Blue | Yellow | Purple 
    deriving (Eq, Ord, Show, Enum, Bounded, Ix)
data CrazyColor = White | Black
    deriving (Eq, Ord, Show, Enum, Bounded, Ix)


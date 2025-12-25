module Color (Color, CrazyColor) where

data Color = Red | Green | Blue | Yellow | Purple 
    deriving (Eq, Ord, Show, Enum, Bounded)
data CrazyColor = White | Black
    deriving (Eq, Ord, Show, Enum, Bounded)


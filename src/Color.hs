module Color (Color(..), CrazyColor(..), getCodeNormal, getCodeCrazy) where

import Data.Ix

data Color = Red | Green | Blue | Yellow | Purple 
    deriving (Eq, Ord, Show, Enum, Bounded, Ix)
data CrazyColor = White | Black
    deriving (Eq, Ord, Show, Enum, Bounded, Ix)

getCodeNormal :: Color -> String

getCodeNormal Red = "\ESC[31m"
getCodeNormal Blue = "\ESC[34m"
getCodeNormal Green = "\ESC[32m"
getCodeNormal Yellow = "\ESC[33m"
getCodeNormal Purple = "\ESC[35m"

getCodeCrazy :: CrazyColor -> String

getCodeCrazy Black = "\ESC[30m"
getCodeCrazy White = "\ESC[37m"

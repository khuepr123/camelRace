module Main where

import GameManager
import Control.Monad
import System.Console.ANSI
import UserInput
import RaceTrack
import ASCII

main :: IO ()
-- main = print $ trackToImage exampleTrack
main = newGame
-- main = do
--     v1 <- chooseAmongst show ["1", "2"]
--     v2 <- chooseAmongst show["11", "22"]
--     print (v1, v2)


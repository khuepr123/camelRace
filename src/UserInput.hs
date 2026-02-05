module UserInput (getAction, ensureInput) where

import RaceTrack (trackRange, Tile, Spectator(Push, Pull))
import Utility
import Color (Color)
import qualified CamelRace as CR
import System.IO
import Text.Read
import Control.Monad
import Data.Ix(inRange)

ensureInput :: IO (Either String b) -> IO b
ensureInput action = do
    badVal <- action
    case badVal of
      Left err -> do
          print err
          putStrLn "Try again:"
          ensureInput action
      Right v -> return v

(!?) :: [a] -> Int -> Either String a
(!?) [] _ = Left "Error: Index out of bound"
(h:_) !? 0 = Right h
(_:t) !? n = t !? (n - 1)

chooseAmongst :: (a -> String) -> [a] -> IO a
chooseAmongst showChoice options = ensureInput $ do
    let verboses = [" " ++ show i ++ ") " ++ showChoice act | (i, act) <- zip [1..] options]
    mapM_ print verboses
    putStr "Your choice? "
    hFlush stdout
    (readEither >=> (options !?)) <$> getLine

-- data Action = BetWinOverall Color | BetLoseOverall Color | BuyTicket Color | RollDice | PlaceSpectator Tile Spectator
data PreAction = BetWinOverall | BetLoseOverall | BuyTicket | RollDice | PlaceSpectator
    deriving (Enum, Bounded)

instance Show PreAction where
    show BetWinOverall = "Bet the overall winning camel"
    show BetLoseOverall = "Bet the overall losing camel"
    show BuyTicket = "Choose a camel to gamble on"
    show RollDice = "Roll the dice"
    show PlaceSpectator = "Place a spectator tile"

getColor :: IO Color
getColor = chooseAmongst show allValues

getTile :: IO Tile
getTile = ensureInput $ do
    s <- getLine
    return $ do 
        val <- readEither s
        if inRange trackRange val
           then Right val
           else Left "Error: Choose a tile that's within the race track"

getSpectator :: IO Spectator
getSpectator = chooseAmongst showSpec [Push, Pull]
    where showSpec Push = "Push (forward 1 tile)"
          showSpec Pull = "Pull (backward 1 tile)"

getAction :: IO CR.Action
getAction = do
    action <- chooseAmongst show allValues
    case action of
      BetWinOverall -> CR.BetWinOverall <$> getColor
      BetLoseOverall -> CR.BetLoseOverall <$> getColor
      BuyTicket -> CR.BetLoseOverall <$> getColor
      RollDice -> return CR.RollDice
      PlaceSpectator -> liftM2 CR.PlaceSpectator getTile getSpectator

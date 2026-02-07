module UserInput (getAction, ensureInput, chooseAmongst, deleteLastNLines, clearScreenFull, printBold) where

import RaceTrack (trackRange, Tile, Spectator(Push, Pull))
import Utility
import Color (Color)
import qualified CamelRace as CR
import System.IO
import Text.Read
import Control.Monad
import Data.Ix(inRange)
import System.Console.ANSI(cursorUpLine, clearFromCursorToScreenEnd, cursorUp, clearScreen, ConsoleIntensity( BoldIntensity ), SGR(..), setSGR)

printBold :: String -> IO ()
printBold s = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn s
    setSGR [Reset]

ensureInput :: IO (Either String b) -> IO b
ensureInput action = do
    badVal <- action
    case badVal of
      Left err -> do
          putStrLn err
          putStrLn "Try again:"
          ensureInput action >>= deleteLastNLines 2

      Right v -> return v

(!?) :: [a] -> Int -> Either String a
(!?) [] _ = Left "Error: Index out of bounds"
(h:_) !? 0 = Right h
(_:t) !? n = t !? (n - 1)

chooseAmongst :: (a -> String) -> [a] -> IO a
chooseAmongst showChoice options = ensureInput $ do
    let verboses = [ " " ++ show i ++ ") " ++ showChoice act
                   | (i, act) <- zip [1..] options
                   ]
    mapM_ putStrLn verboses
    putStr "Your choice? "
    hFlush stdout
    -- getLine >>= delOption . indexingEitherFromString
    indexingEitherFromString <$> getLine
    where -- delOption = deleteLastNLines (length options + 1)
          indexingEitherFromString = readEither >=> (options !?) . subtract 1

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
    putStr "Choose a tile number from 1 to 16: "
    hFlush stdout
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

clearScreenFull :: IO ()
clearScreenFull = clearScreen >> cursorUp maxBound

getAction :: IO CR.Action
getAction = do
    action <- chooseAmongst show allValues
    case action of
      BetWinOverall -> CR.BetWinOverall <$> getColor
      BetLoseOverall -> CR.BetLoseOverall <$> getColor
      BuyTicket -> CR.BuyTicket <$> getColor
      RollDice -> return CR.RollDice
      PlaceSpectator -> liftM2 CR.PlaceSpectator getTile getSpectator

deleteLastNLines :: Int -> a -> IO a
deleteLastNLines n val = do
    cursorUpLine n
    clearFromCursorToScreenEnd
    return val

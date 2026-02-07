module GameManager where

import Text.Read(readEither)
import Control.Monad.State
-- import Control.Monad.Trans.Either
import Control.Lens
import UserInput
import CamelRace
import Utility
import RaceTrack(GameStatus(..))
import System.IO(hFlush, stdout)
import System.Random(getStdGen)
import Player(Name, PlayerBase, getName)
import Control.Monad(replicateM, (>=>), replicateM_)
import ASCII
import Data.Ix(inRange)

getPIDname :: PlayerId -> StateT GameState IO Name
getPIDname pID = gets (getName pID . view players)
    
printLastEvent :: StateT GameState IO ()
printLastEvent = do
    eventStack <- gets (view events)
    case eventStack of
        [] -> pure ()
        h:_ -> lift $ printBold (show h)

playIOLog :: PlayerId -> Maybe String -> StateT GameState IO RoundStatus
playIOLog pID message = do
    display
    case message of
        Just s -> lift $ printBold s >> printBold "Try Again: "
        Nothing -> printLastEvent >> lift (putStrLn "")
    name <- getPIDname pID
    lift $ putStrLn $ "Player " ++ name ++ "'s turn: "
    action <- lift getAction
    thisState <- get
    case runStateT (play pID action) thisState of
      Left s -> playIOLog pID (Just s)
      Right (roundStatus, newState) -> do
          put newState
          return roundStatus


playIO :: PlayerId -> StateT GameState IO RoundStatus
playIO pID = playIOLog pID Nothing


runRound :: PlayerId -> StateT GameState IO GameStatus
runRound pID = do
    roundStatus <- playIO pID
    gameStatus <- gameEnded
    case (gameStatus, roundStatus) of
      (Concluded, _) -> return Concluded
      (_, Ended)     -> return Progressing
      _ -> getNextPlayer pID >>= runRound

runFromPlayer :: PlayerId -> StateT GameState IO PlayerBase
runFromPlayer pID = do
    status <- runRound pID
    case status of
      Concluded   -> cleanupGame
      Progressing -> getNextPlayer pID >>= runFromPlayer



display :: StateT GameState IO ()
display = do
    playerBase <- gets (view players)
    lift clearScreenFull
    lift (traverse print playerBase)
    track <- gets (fst . view randomTrack)
    lift $ print (trackToImage track)
    hall <- gets (view ticketHall)
    lift $ print (ticketHallToImage hall)


-- withRNG :: MonadState GameState m => State StdGen b -> m b
-- play :: PlayerId -> Action -> StateT GameState (Either String) RoundStatus

readNames :: Int -> IO [Name]
readNames count = do
    putStrLn "Enter all player names, seperated by line, in playing order:"
    hFlush stdout
    replicateM count getLine


getPlayerNum :: IO Int
getPlayerNum = ensureInput $ do
    putStr "Enter number of players: "
    hFlush stdout
    (readEither >=> isSatNumPlayer) <$> getLine
    where isSatNumPlayer :: Int -> Either String Int
          isSatNumPlayer x
            | inRange playerRange x = Right x
            | otherwise             = Left $ "number of player not in range " ++ show playerRange
          playerRange = (2, 8)


initGameState :: IO GameState
initGameState = do
    clearScreenFull
    seed <- getStdGen
    names <- getPlayerNum >>= readNames
    return $ newGameState names seed



newGame :: IO ()
newGame = do
    initGameState >>= runStateT (runFromPlayer 0 >> display)
    pure ()
    




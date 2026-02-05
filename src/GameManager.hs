module GameManager where

import Text.Read(readEither)
import Control.Monad.State
import Control.Monad.Trans.Either
import UserInput
import CamelRace
import Utility
import RaceTrack(GameStatus(..))
import System.IO(hFlush, stdout)
import Player(Name)


playIO :: PlayerId -> StateT GameState IO RoundStatus
playIO pID = do
    action <- lift getAction
    thisState <- get
    case runStateT (play pID action) thisState of
      Left s -> lift (putStrLn s) >> playIO pID
      Right (roundStatus, newState) -> do
          put newState
          return roundStatus
    

runGameRound :: PlayerId -> StateT GameState IO GameStatus
runGameRound pID = do
    roundStatus <- playIO pID
    gameStatus <- gameEnded
    case (gameStatus, roundStatus) of
      (Concluded, _) -> return Concluded
      (_, Ended)     -> return Progressing
      _ -> getNextPlayer pID >>= runGameRound
    
    
-- withRNG :: MonadState GameState m => State StdGen b -> m b
-- play :: PlayerId -> Action -> StateT GameState (Either String) RoundStatus

readNames :: IO [Name]
readNames = ensureInput $ runEitherT $ do
    Right []
    

initGameState :: IO GameState
initGameState = do
    seed <- getStdGen
    putStr "Enter number of players: "
    hFlush stdout
    
    

newGame :: IO ()
newGame = do
    error "unimplemented"
    

module RandomTrack where 

import RaceTrack
import Utility
import System.Random
import Control.Monad.State
import Data.List.NonEmpty(NonEmpty(..), fromList)

jumpRange :: (Int, Int)
jumpRange = (1, 3)

type RandomTrack = (RaceTrack, NonEmpty MoveUnit)

roll :: RandomTrack -> (Maybe PlayerId, Either RaceTrack RandomTrack, Camel)
roll (track, move :| moves) = (mayPID, ,camel) $ case moves of
                        []                 -> Left updatedTrack
                        otherMove : others -> Right (updatedTrack, otherMove :| others)
    where (mayPID, updatedTrack) = updateTrack move track
          camel = fst move


randInRange :: (Int, Int) -> State StdGen Int
randInRange = state . uniformR


genMove :: Camel -> State StdGen MoveUnit 
genMove camel = (camel,) <$> randInRange jumpRange


genTrack :: State StdGen RandomTrack 
genTrack = do
    moves <- mapM genMove allCamels
    restock (newTrack moves)


randomDrop :: [a] -> State StdGen [a]
randomDrop ls = do
    index <- randInRange (0, length ls - 1)
    return $ take index ls ++ drop (index + 1) ls


restock :: RaceTrack -> State StdGen RandomTrack
restock track = do
    let normals = map Normal allValues
        crazys = map Crazy allValues
    crazyDrop <- randomDrop crazys
    drops     <- randomDrop (normals ++ crazyDrop)
    moves <- mapM genMove drops
    return (track, fromList moves)

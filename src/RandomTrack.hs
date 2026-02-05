module RandomTrack where 

import Color
import Ranking
import RaceTrack
import Utility
import System.Random
import Control.Monad.State

jumpRange :: (Int, Int)
jumpRange = (1, 3)

type RandomTrack = (RaceTrack, [MoveUnit])

roll :: RandomTrack -> Either RaceTrack (Maybe PlayerId, RandomTrack)
roll (track, moves) = case moves of
                        []            -> Left track
                        move : others -> Right (mayPID, (updatedTrack, others))
                                           where (mayPID, updatedTrack) = updateTrack move track


randInRange :: (Int, Int) -> State StdGen Int
randInRange = state . uniformR


genMove :: Camel -> State StdGen MoveUnit 
genMove camel = (camel,) <$> randInRange jumpRange


genTrack :: State StdGen RandomTrack 
genTrack = do
    moves <- mapM genMove allCamels
    return (newTrack moves, [])


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
    return (track, moves)

getRanking :: RandomTrack -> Ranking Color
getRanking = getRankingTrack . fst

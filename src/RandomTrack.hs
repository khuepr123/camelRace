module MoveGen () where

-- module RaceTrack (RaceTrack, MoveUnit, Spectator, newTrack, updateTrack, getRanking, addSpectator, deleteSpectator, ongoing) where
import RaceTrack
import Utility
import System.Random
import Data.List
import Control.Monad.State
import Control.Monad

jumpRange = (1, 3)

type RandomTrack = (RaceTrack, [MoveUnit])

roll :: RandomTrack -> Either RaceTrack RandomTrack
roll (track, moves) = case moves of
                        []            -> Left track
                        move : others -> Right (updateTrack move track, others)
                       
-- unfoldrEither :: (a -> Either b a) -> a -> [a]
-- unfoldrEither f = unfoldr (transform f)
--     where transform (Left _) = Nothing
--           transform (Right v) = Just (v, v)

-- genMove :: StdGen -> Camel -> (StdGen, MoveUnit)
-- genMove gen camel = let (jmp, newGen) = uniformR jumpRange gen
--                      in (newGen, (camel, jmp))

randInRange :: (Int, Int) -> State StdGen Int
randInRange = state . uniformR

genMove :: Camel -> State StdGen MoveUnit 
genMove camel = (camel,) <$> randInRange jumpRange

-- genTrack :: StdGen -> (RandomTrack, StdGen)
-- genTrack gen = let (newGen, moves) = mapAccumL genMove gen allCamels
--                 in ((newTrack moves, []), newGen)

genTrack :: State StdGen RandomTrack 
genTrack = do
    moves <- mapM genMove allCamels
    return (newTrack moves, [])

-- randomDrop :: [a] -> StdGen -> ([a], StdGen)
-- randomDrop ls gen = (take index ls ++ drop (index + 1) ls, newGen)
--   where (index, newGen) = uniformR (0, length ls - 1) gen

randomDrop :: [a] -> State StdGen [a]
randomDrop ls = do
    index <- randInRange (0, length ls - 1)
    return $ take index ls ++ drop (index + 1) ls
      
-- normals = map Normal allValues
-- crazys = map Crazy allValues

-- restock :: RaceTrack -> StdGen -> (RandomTrack, StdGen)
-- restock track gen0 = 
--     let (crazyDrop, gen1) = randomDrop crazys gen0
--         (drops, gen2) = randomDrop (normals ++ crazyDrop) gen1
--         (newGen, moves) = mapAccumL genMove gen2 drops
--      in ((track, moves), newGen)

restock :: RaceTrack -> State StdGen RandomTrack
restock track = do
    let normals = map Normal allValues
        crazys = map Crazy allValues
    crazyDrop <- randomDrop crazys
    drops     <- randomDrop (normals ++ crazyDrop)
    moves <- mapM genMove drops
    return (track, moves)

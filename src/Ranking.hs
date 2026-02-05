module Ranking (Ranking, fromList, toList, getFirst, getSecond, getLast) where

import Utility
import qualified Data.Set as S
import Data.List(nub)

newtype Ranking a = Ranking [a]

fromList :: (Bounded a, Enum a, Ord a) => [a] -> Ranking a
fromList l
  | nub l /= l = error "duplicate elements"
  | S.fromList allValues /= S.fromList l = error "incomplete element set"
  | otherwise = Ranking l

toList :: Ranking a -> [a]
toList (Ranking l) = l

getFirst :: Ranking a -> a
getFirst rank = head (toList rank)

getSecond :: Ranking a -> a
getSecond rank = toList rank !! 1

getLast :: Ranking a -> a
getLast rank = last (toList rank)

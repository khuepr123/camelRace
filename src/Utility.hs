module Utility where

import Data.Ix

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

rangeDef :: (Bounded a, Ix a) => (a, a)
rangeDef = (minBound, maxBound)

type PlayerId = Int


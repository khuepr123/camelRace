module Utility where

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

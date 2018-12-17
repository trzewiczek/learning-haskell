module Lesson_13 where

class Describable a where 
  describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- Q13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n
              
module Lesson_17 where
import Data.Semigroup

myAny :: (a -> Bool) -> [a] -> Bool
myAny predicate = foldl (||) False . (map predicate)


data Color =
    Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | White
  deriving (Show, Eq)

instance Semigroup Color where
  (<>) White a = a
  (<>) a White = a
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Red Yellow = Orange
  (<>) Yellow Red = Orange
  (<>) a b | a == b = a
           | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
           | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
           | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
           | otherwise = Brown

instance Monoid Color where
  mempty = White
  mappend = (<>)

-- -----------------------------------

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1s = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1s
        cycledL2 = cycle l2

data Events = Events [ String ]
instance Semigroup Events where
  (<>) events (Events []) = events
  (<>) (Events []) events = events
  (<>) (Events e1) (Events e2) = Events $ cartCombine combiner e1 e2
    where combiner x y = mconcat [ x, "-", y ]

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

data Probs = Probs [ Double ]
instance Semigroup Probs where
  (<>) probs (Probs []) = probs
  (<>) (Probs []) probs = probs
  (<>) (Probs p1) (Probs p2) = Probs $ cartCombine (*) p1 p2

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events (Probs normalizedProps)
  where normalizedProps = map (\x -> x / totalProbs) probs
        totalProbs = sum probs

showPair :: Int -> String -> Double -> String
showPair maxWidth event prob = mconcat [ event, pad, " | ", show prob, "\n" ]
  where pad = replicate missing ' '
        missing = maxWidth - length event

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat rows
    where rows = zipWith (showPair maxWidth) events probs
          maxWidth = maximum $ map length events


instance Semigroup PTable where
  (<>) ptable (PTable (Events []) (Probs [])) = ptable
  (<>) (PTable (Events []) (Probs [])) ptable = ptable
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = e1 <> e2
          newProbs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

-- ---------------

coin :: PTable
coin = createPTable
       (Events [ "heads", "tails" ])
       (Probs  [     0.5,     0.5 ])

spinner :: PTable
spinner = createPTable
          (Events [ "red", "blue", "green" ])
          (Probs  [   0.1,    0.2,     0.7 ])


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

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProps
  where normalizedProps = map (\x -> x / totalProbs) probs
        totalProbs = sum probs

showPair :: Int -> String -> Double -> String
showPair maxWidth event prob = mconcat [ event, pad, " | ", show prob, "\n" ]
  where pad = replicate missing ' '
        missing = maxWidth - length event

instance Show PTable where
  show (PTable events probs) = mconcat rows
    where rows = zipWith (showPair maxWidth) events probs
          maxWidth = maximum $ map length events


cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1s = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1s
        cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner x y = mconcat [ x, "-", y ]

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable (PTable [] []) = ptable
  (<>) (PTable [] []) ptable = ptable
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable [ "red", "blue", "green" ] [ 0.1, 0.2, 0.7 ]


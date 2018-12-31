module Lesson_20 where

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]


-- Time Series

data TS a = TS [Int] [Maybe a]

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time Nothing = mconcat [ show time, " | NA\n" ]
showTVPair time (Just value) = mconcat [ show time, " | ", show value, "\n" ]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values


createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where completeTimes = [ minimum times .. maximum times ]
        timeValueMap = Map.fromList (zip times values)
        extendedValues = map (\t -> Map.lookup t timeValueMap) completeTimes

fileToTS :: [(Int, Double)] -> TS Double
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs


insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap


combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes = mconcat [ t1, t2 ]
        completeTimes = [ minimum bothTimes .. maximum bothTimes ]
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
        combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)


-- test data

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

tsAll :: TS Double
tsAll = mconcat [ ts1, ts2, ts3, ts4 ]


-- TS comparisons

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newFun
  where newFun (idA, Nothing) (idB, Nothing) = (idA, Nothing)
        newFun (_, Nothing) (idB, b) = (idB, b)
        newFun (idA, a) (_, Nothing) = (idA, a)
        newFun (idA, Just a) (idB, Just b) = if f a b == a
                                             then (idA, Just a)
                                             else (idB, Just b)

compareTS :: Eq a => CompareFunc a -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just best
  where pairs = zip times values
        best = foldl1 (makeTSCompare func) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max


-- TS diffing

type PairFunction a = a -> a -> a
type DiffPairFunction a = Maybe a -> Maybe a -> Maybe a
type DiffTSFunction a = TS a -> TS a

makeDiffTS :: DiffPairFunction a -> DiffTSFunction a
makeDiffTS diffPair = diffTS
  where diffTS (TS [] []) = TS [] []
        diffTS (TS times values) = TS times (Nothing : diffedValues)
          where shiftedValues = tail values
                diffedValues = zipWith diffPair shiftedValues values

makeDiffPair :: PairFunction a -> DiffPairFunction a
makeDiffPair pairFunc = diffPair
  where diffPair _ Nothing = Nothing
        diffPair Nothing _ = Nothing
        diffPair (Just v1) (Just v2) = Just (v1 `pairFunc` v2)


diffTS :: Num a => TS a -> TS a
diffTS = makeDiffTS $ makeDiffPair (-)

divTS :: Fractional a => TS a -> TS a
divTS = makeDiffTS $ makeDiffPair (/)


-- TS mean / median stats

type StatFunction a b = [Maybe a] -> Maybe b
type MovingStatFunction a b = [Maybe a] -> Int -> [Maybe b]
type MovingStatFunctionTS a b = TS a -> Int -> TS b

makeMovingStat :: StatFunction a b -> MovingStatFunction a b
makeMovingStat stat = movingStat
  where movingStat [] _ = []
        movingStat xs n = if length window == n
                          then stat window : movingStat restXS n
                          else []
          where window = take n xs
                restXS = tail xs

makeMovingStatTS :: StatFunction a b -> MovingStatFunctionTS a b
makeMovingStatTS stat = movingStatTS
  where movingStatTS (TS [] []) n = (TS [] [])
        movingStatTS (TS times values) n = TS times smoothedValues
          where ma = (makeMovingStat stat) values n
                nas = replicate (n `div` 2) Nothing
                smoothedValues = mconcat [ nas, ma, nas ]


-- mean

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs


meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe [] = Nothing
meanMaybe xs = if any (== Nothing) xs
               then Nothing
               else Just result
  where result = mean $ map fromJust xs


movingMean :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingMean = makeMovingStat meanMaybe

movingMeanTS :: Real a => TS a -> Int -> TS Double
movingMeanTS = makeMovingStatTS meanMaybe

-- median

median :: (Ord a, Fractional a) => [a] -> a
median xs = if odd $ length xs
            then lowerHalf
            else (lowerHalf + upperHalf) / 2
  where values = sort xs
        half = (length values) `div` 2
        lowerHalf = values !! half
        upperHalf = values !! (half + 1)


medianMaybe :: (Ord a, Fractional a) => [Maybe a] -> Maybe a
medianMaybe [] = Nothing
medianMaybe xs = if any (== Nothing) xs
                 then Nothing
                 else Just result
  where result = median $ map fromJust xs

movingMedian :: (Ord a, Fractional a) => [Maybe a] -> Int -> [Maybe a]
movingMedian = makeMovingStat medianMaybe

movingMedianTS :: (Ord a, Fractional a) => TS a -> Int -> TS a
movingMedianTS = makeMovingStatTS medianMaybe

-- SD

standardDeviation :: (Real a, Fractional a) => [a] -> Double
standardDeviation xs = sqrt variance
  where xsMean = realToFrac $ mean xs
        distances = map ((-) xsMean) xs
        variance = mean $ map (^2) distances


standardDeviationMaybe :: (Real a, Fractional a) => [Maybe a] -> Maybe Double
standardDeviationMaybe [] = Nothing
standardDeviationMaybe xs = if any (== Nothing) xs
                            then Nothing
                            else Just result
  where result = standardDeviation $ map fromJust xs


standardDeviationTS :: (Real a, Fractional a) => TS a -> Maybe Double
standardDeviationTS (TS [] []) = Nothing
standardDeviationTS (TS times values) = standardDeviationMaybe values

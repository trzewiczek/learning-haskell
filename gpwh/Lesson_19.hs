module Lesson_19 where
import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Organ =
    Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [ (Int, Organ) ]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents id = Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                      (\e -> e == Just organ)
                                      available)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isJust availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

-- -------------------------

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just x) = x

-- -------------------------

data Container =
    Vat Organ
  | Cooler Organ
  | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in vat"
  show (Cooler organ) = show organ ++ " in cooler"
  show (Bag organ) = show organ ++ " in bag"

data Location =
    Lab
  | Kitchen
  | Bathroom
  deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation $ organToContainer organ

report :: (Location, Container) -> String
report (location, container) = show container ++
                               " in the " ++
                               show location

processAndReport :: (Maybe Organ) -> String
processAndReport Nothing = "Error, id not found"
processAndReport (Just organ) = report $ process organ

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalogue = processAndReport organ
  where organ = Map.lookup id catalogue

-- a common pattern in Haskell: you separate the parts of the code for which
-- you need to worry about a problem (for example, missing values) from the
-- ones that you don’t.

-- Q19.1
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers organsList = length $ filter isNothing organsList

-- Q19.2
maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap _ [] = []
maybeMap f (x:xs) = fun x : (maybeMap f xs)
  where fun Nothing = Nothing
        fun (Just a) = Just (f a)

module Lesson_18 where
import qualified Data.Map as Map


data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box a) = a

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

-- -------------------------

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 0.2 0.3

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [ x, y, z ]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)


data List a = Empty | Cons a (List a) deriving Show

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap f (Cons head rest) = Cons (f head) (ourMap f rest)

-- -------------------------

data Organ =
    Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Show, Eq, Ord)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [ (Int, Organ) ]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Organ Int
organInventory = Map.fromList $ zip organKeys organCounts
  where organKeys = [ Heart, Brain, Kidney, Spleen ]
        organCounts = [ 2, 1, 1, 2 ]

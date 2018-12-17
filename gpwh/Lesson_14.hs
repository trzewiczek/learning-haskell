module Lesson_14 where

data NewEngland = ME | VT | NH | MA | RI | CT

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)
instance Show SixSidedDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"

newtype Name = Name (String, String) deriving (Eq, Show)
instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [ Name ("Emil", "Cioran")
        , Name ("Eugene", "Thacker")
        , Name ("Friedrich", "Nietsche") ]

-- Q14.1
data Number = One | Two | Three deriving (Enum, Show)
instance Eq Number where
  (==) num1 num2 = fromEnum num1 == fromEnum num2
instance Ord Number where
  compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

--Q14.2
class Enum a => Die a where
  iconize :: a -> String

data FiveSidedDie = D1 | D2 | D3 | D4 | D5 deriving (Enum, Eq, Ord, Show)
instance Die FiveSidedDie where
  iconize die = replicate (fromEnum die + 1) '*'


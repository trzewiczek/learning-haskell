module Lesson_16 where

-- ---------------------------

type FirstName = String
type MiddleName = String
type LastName = String

data Name =
    Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  deriving Show

data Creator =
    AuthorCreator Author
  | ArtistCreator Artist
  deriving Show

data Author = Author Name deriving Show

data Artist =
    Person Name
  | Band String
  deriving Show


data Book = Book {
    author     :: Creator
  , isbn       :: String
  , bookTitle  :: String
  , bookYear   :: Int
  , bookPrice  :: Double }

data VinylRecord = VinylRecord {
    artist       :: Creator
  , recordTitle  :: String
  , recordYear   :: Int
  , recordPrice  :: Double }

data CollectibleToy = CollectibleToy {
    name         :: String
  , description  :: String
  , toyPrice     :: Double }

data Pamphlet = Pamphlet {
    pamphletTitle       :: String
  , pamphletDescription :: String
  , organization        :: String
  , pamphletPrice       :: Double }

data StoreItem =
    BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

madeBy :: StoreItem -> String
madeBy (BookItem book) = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy _ = "No nothing"

hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                (Author
                  (TwoInitialsWithLast 'H' 'P' "Lovecraft"))


-- Q16.2
data Shape =
    Circle Double
  | Rectangle Double Double
  | Square Double

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle a b) = 2 * a + 2 * b
perimeter (Square a) = 4 * a

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b
area (Square a) = a^2

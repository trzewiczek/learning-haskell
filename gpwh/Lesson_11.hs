module Lesson_11 where

-- Quick check 11.1

halve :: Integer -> Integer
halve x = x `div` 2

-- Quick check 11.2

printDouble :: Int -> String
printDouble x = show $ x * 2
 

-- Quick chceck 11.3

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street city = (number, street, city)

-- Int -> String -> String -> (Int, String, String)
-- String -> String -> (Int, String, String)
-- String -> (Int, String, String)
-- (Int, String, String)


-- Q11.1
-- What is the type signature for filter? How is it different from map?

-- filter :: (a -> Bool) -> [a] -> [a]


-- Q11.2

-- In Haskell, both tail and head have an error when called on an empty list. You 
-- can write a version of tail that won’t fail but instead return an empty list 
-- when called on an empty list. Can you write a version of head that returns 
-- an empty list when called on an empty list? To answer this, start by writing 
-- out the type signatures of both head and tail.

-- No way to write a signature for head that returns either a value or an empty list.


-- Q11.3

-- Recall myFoldl from lesson 9.

-- myFoldl f init [] = init
-- myFoldl f init (x:xs) = myFoldl f newInit xs
--   where newInit = f init x

-- What’s the type signature of this function? Note: foldl has a different type signature. 

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

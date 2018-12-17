> module Chapter_01 where
> import Euterpea

> simple :: Int -> Int -> Int -> Int
> simple x y z = x * (y + z)

Exercise 1.1
-- simple (simple 2 3 4) 5 6
-- simple (2 * (3 + 4)) 5 6
-- simple (2 * 7) 5 6
-- simple 14 5 6
-- 14 * (5 + 6)
-- 14 * 11
-- 154


Exercise 1.2
-- ???


Exercise 1.3

-- [ A, B, C ]
   passes, list of PitchClasses

-- [ D, 42 ]
   fails, mixed nature of the values on the list

-- (-42, Ef)
   passes, pair of Int and PitchClass

-- [ ('a', 3), ('b', 5) ]
   passes, list of pairs of Chars and Ints

-- simple 'a' 'b' 'c'
   fails, called with Cahrs instead of Ints

-- (simple 1 2 3, simple)
   passes, pair of Int and function

-- [ "I", "love", "Euterpea" ]
   passes, list of Strings (a.k.a. [Char])


Exercise 1.4

> hNote :: Dur -> Pitch -> AbsPitch -> Music Pitch
> hNote d p ap = note d p :=: note d (trans ap p)
>
> hList :: Dur -> [Pitch] -> AbsPitch -> Music Pitch
> hList d [] ap = rest 0
> hList d (p:ps) ap = hNote d p ap :+: hList d ps ap

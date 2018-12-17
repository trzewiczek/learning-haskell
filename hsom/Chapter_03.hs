module Chapter_03 where
import Euterpea

-- Exercise 3.1
f1 :: Int -> [Pitch] -> [Pitch]
f1 t ps = let f = trans t
          in map f ps


f2 :: [Dur] -> [Music a]
f2 ds = let f d = Prim $ Rest d
        in map f ds


f3 :: [Music Pitch] -> [Music Pitch]
f3 xs = let f (Prim (Note d n)) = note (d / 2) n :+: rest (d / 2)
        in map f xs

reverse :: [a] -> [a]
reverse xs = let rev acc [] = acc
                 rev acc (x:xs) = rev (x:acc) xs
             in rev [] xs



main = play $ c 4 qn
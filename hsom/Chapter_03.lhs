> module Chapter_03 where
> import Euterpea

Exercise 3.1
------------

> f1 :: Int -> [Pitch] -> [Pitch]
> f1 t ps = let f = trans t
>           in map f ps


> f2 :: [Dur] -> [Music a]
> f2 ds = let f d = Prim $ Rest d
>         in map f ds


> f3 :: [Music Pitch] -> [Music Pitch]
> f3 xs = let f (Prim (Note d n)) = note (d / 2) n :+: rest (d / 2)
>         in map f xs


> reverse :: [a] -> [a]
> reverse xs = let rev acc [] = acc
>                  rev acc (x:xs) = rev (x:acc) xs
>              in rev [] xs


Exercise 3.2
------------
flip (flip f)
flip (flip (f x y))
flip (f y x)
f x y


Exercise 3.3
------------

> xs = [ 1, 2, 3 ] :: [ Integer ]
> ys :: [ Integer -> Integer ]
> ys = map (+) xs


Exercise 3.4
------------

> applyEach :: [ a -> b ] -> a -> [ b ]
> applyEach fs x = map (\f -> f x) fs 


Exercise 3.5
------------

> applyAll :: [ a -> a ] -> a -> a
> applyAll fs x = foldl (\acc f -> f acc) x fs


Exercise 3.6
------------

> appendr, appendl :: [[a]] -> [a]
> appendr = foldr (flip (++)) []
> appendl = foldl (flip (++)) []

01:  appendr [ [ 1, 2 ], [ 3, 4 ] ]
02:  foldr (flip (++)) [] [ [ 1, 2 ], [ 3, 4 ]]
03:  [ 1, 2 ] `(flip (++))` ([ 3, 4 ] `(flip (++))` [])
04:  [ 1, 2 ] `(flip (++))` ([] ++ [ 3, 4 ])
05:  [ 1, 2 ] `(flip (++))` [ 3, 4 ]
06:  [ 3, 4 ] ++ [ 1, 2 ]
07:  3 : ([ 4 ] ++ [ 1, 2 ])
08:  3 : 4 : [ 1, 2 ]
09:  3 : [ 4, 1, 2 ]
10:  [ 3, 4, 1, 2 ]

01:  appendl [ [ 1, 2 ], [ 3, 4 ] ]
02:  foldl (flip (++)) [] [ [ 1, 2 ], [ 3, 4 ]]
03:  ([] `(flip (++))` [ 1, 2 ]) `(flip (++))` [ 3, 4 ]
04:  ([ 1, 2 ] ++ []) `(flip (++))` [ 3, 4 ]
05:  (1 : ([ 2 ] ++ [])) `(flip (++))` [ 3, 4 ]
06:  (1 : 2 : []) `(flip (++))` [ 3, 4 ]
07:  (1 : [ 2 ]) `(flip (++))` [ 3, 4 ]
08:  [ 1, 2 ] `(flip (++))` [ 3, 4 ]
09:  [ 3, 4 ] ++ [ 1, 2 ]
10:  3 : ([ 4 ] ++ [ 1, 2 ])
11:  3 : 4 : [ 1, 2 ]
12:  3 : [ 4, 1, 2 ]
13:  [ 3, 4, 1, 2 ]


Exercise 3.7
------------

> lengthNR :: [ a ] -> Integer
> lengthNR = foldl (\acc _ -> acc + 1) 0


Exercise 3.8
------------


























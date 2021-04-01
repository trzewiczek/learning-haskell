-- QCh4.1
-- Write a lambda function for cubing x and pass it to ifEven.

isEven myF x = if even x
               then myF x
               else x

-- Main> isEven (\x -> x^3) 2
-- 8
-- Main> isEven (\x -> x^3) 3
-- 3


-- QCh4.2
-- In compareLastNames, you didn’t handle the case of having two last names
-- that are the same but with different first names. Modify the compareLastNames
-- function to compare first names and use it to fix compareLastNames.

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else if firstName1 > firstName2
                                         then GT
                                         else if firstName1 < firstName2
                                              then LT
                                              else EQ
  where firstName1 = fst name1
        lastName1  = snd name1
        firstName2 = fst name2
        lastName2  = snd name2

-- Q4.1
-- Anything that can be compared in Haskell (for example, [Char], which you use
-- for the names in your name tuples) can be compared with a function called compare.
-- The compare function returns GT, LT, or EQ. Rewrite compareLastNames by using compare.

compareLastNamesBis name1 name2 = if lastNames == EQ
                                  then firstNames
                                  else lastNames
  where firstNames = compare (fst name1) (fst name2)
        lastNames  = compare (snd name1) (snd name2)


-- Q4.2
-- Define a new location function for Washington, DC and add it to getLocationFunction.
-- In the DC function, everyone’s names must be followed by Esq.

dcOffice name = nameText ++ ", Esq., PO Box 852 - Washington, DC, 54876"
  where nameText = fst name ++ " " ++ snd name

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV, 89523"
  where nameText = snd name

sfOffice name = if lastName < "L"
                then nameText ++
                     " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++
                     " - PO Box 1010 - San Francisco, CA, 94109"
  where lastName = snd name
        nameText = fst name ++ " " ++ lastName

getLocation location  = case location of
  "dc"   -> dcOffice
  "ny"   -> nyOffice
  "reno" -> renoOffice
  "sf"   -> sfOffice
  _      -> (\name -> fst name ++ " " ++ snd name)

addressLetter name location = locationF name
  where locationF = getLocation location

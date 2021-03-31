-- QCh3.1
-- Write a lambda function that doubles its argument, 
-- and pass in a few num-bers as arguments.

-- (\x -> x * 2) 5

-- QCh3.2
-- Rewrite the following function to use a lambda function in place of where:
-- doubleDouble x = dubs*2 
--   where dubs = x*2

doubleDouble x = (\dubs -> dubs * 2) (x * 2)


-- QCh3.3
-- Redefine overwrite by using only lambdas.

overwrite x = (\x -> (\x -> (\x -> x) 4) 3) 2

-- Q3.1
sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare 
                           else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x + y)^2

sumSquareOrSquareSumLambda = (\x y -> 
                               (\sumSquare squareSum -> 
                                 if sumSquare > squareSum
                                 then sumSquare 
                                 else squareSum) (x^2 + y^2) (x + y)^2)

-- Q3.2
counter x = (\x -> (\x -> x) (x + 1)) (x + 1)
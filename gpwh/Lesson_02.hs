-- QCh2.1
-- Many languages use the ++ operator to increment a value;
-- for example, x++ increments x. Do you think Haskell has
-- an operator or function that works this way?

-- No. Haskell values are immutable.


-- QCh2.2
-- Even languages that don’t have a ++ operator allow for
-- a += operator, often also used for incrementing a value.
-- For example, x += 2 increments x by 2. You can think of
-- += as a function that follows our rules: it takes a value
-- and returns a value. Does this mean += can existin Haskell?

-- No. Haskell values are immutable.


-- QCh2.3

doublePlusTwo x = doubleX + 2
  where doubleX = x * x

-- QCh2.4
-- What’s the final value of the x variable in the following code?
-- GHCi> let x = simple simple
-- GHCi> let x = 6

-- 6


-- Q2.1
-- You used Haskell’s if then else expression to write calcChange.
-- In Haskell, all if statements must include an else component.
-- Given our three rules for functions, why can’t you have an if
-- statement all by itself?

-- Because it would make the function incomlete and return no value
-- for a subset of results.


-- Q2.2
-- Write functions named inc, double, and square that increment,
-- double, and square an argument n, respectively.

inc    x = x + 1
double x = x * 2
square x = x * x

-- Q2.3
-- Write a function that takes a value n. If n is even,
-- the function returns n - 2, and if the number is odd,
-- the function returns 3 × n + 1. To check whether the
-- number is even, you can use either Haskell’s even
-- function or mod (Haskell’s modulo function).

q23 n = if even n
        then n - 2
        else 3 * n + 1

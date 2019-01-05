module Main where
import qualified Data.Map as Map

-- Q21.1

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

inputData :: Map.Map Int String
inputData = Map.singleton 1 "John"

maybeMain :: Maybe String
maybeMain = do
  userName <- Map.lookup 1 inputData
  let greeting = helloPerson userName
  return greeting

-- Q21.2

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
  putStrLn "Input a number:"
  putStr "> "
  userNumber <- getLine
  let result = fib $ read userNumber
  putStrLn $ show result

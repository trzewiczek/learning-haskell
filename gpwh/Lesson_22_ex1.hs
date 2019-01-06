module Main where
import Control.Monad
import System.Environment

calc :: [String] -> Int
calc (x:"+":y:rest) = read x + read y
calc (x:"*":y:rest) = read x * read y

main :: IO ()
main = do
  userInput <- getContents
  let expressions = lines userInput
  let results = map (calc . words) expressions
  mapM_ print results

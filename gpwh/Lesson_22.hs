module Main where
import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print $ sum ints

-- QCh22.2
myReplicate :: Monad m => Int -> m a -> m [a]
myReplicate n action = mapM (\_ -> action) [1..do]

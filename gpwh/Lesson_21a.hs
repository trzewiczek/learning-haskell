module Main where

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, price) = price / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, price) = "The " ++ show size ++ " pizza " ++
                              "is cheaper at " ++
                              show costSqInch ++
                              " per square inch."
  where costSqInch = costPerInch (size, price)


main :: IO ()
main = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine

  putStrLn "What is the price of pizza 1"
  price1 <- getLine

  putStrLn "What is the size of pizza 2"
  size2 <- getLine

  putStrLn "What is the price of pizza 2"
  price2 <- getLine

  let pizza1 = (read size1, read price1)
  let pizza2 = (read size2, read price2)

  let betterPizza = comparePizzas pizza1 pizza2

  putStrLn $ describePizza betterPizza


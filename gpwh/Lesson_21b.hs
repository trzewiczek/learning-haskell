module Lesson_21b where
import qualified Data.Map as Map

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

sizeData :: Map.Map Int Double
sizeData = Map.fromList [ (1, 20.0), (2, 15.0) ]

priceData :: Map.Map Int Double
priceData = Map.fromList [ (1, 18.0), (2, 16.0) ]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData

  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData

  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)

  let betterPizza = comparePizzas pizza1 pizza2

  return $ describePizza betterPizza



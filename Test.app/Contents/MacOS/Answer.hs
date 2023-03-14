import Test.QuickCheck
import Data.Char
import Test

-- chapter1
-- Q3
addAndMultiAns :: Int -> Int -> Int
addAndMultiAns x y = (x+y)*2*x

-- Q4
isTripleAns :: Int -> Int -> Int -> Bool
isTripleAns x y z = x*x + y*y == z*z

-- chapter2
-- Q3
doubleOddsAns :: [Int] -> [Int]
doubleOddsAns xs = [x*2|x<-xs, mod x 2 == 1]

-- Q4
listToUpperAns :: String -> String
listToUpperAns xs = [toUpper x|x<-xs]

-- Q5
rangeDiffCalcAns :: Int -> Int
rangeDiffCalcAns x | x < 3            = x*x
                   | x >= 3 && x <= 5 = x*2
                   | otherwise        = x-5

-- chapter3
-- Q2
doublesRecAns :: [Int] -> [Int]
doublesRecAns [] = []
doublesRecAns (x:xs) = x*2:doublesRecAns xs

-- Q3
prodRecAns :: [Int] -> Int
prodRecAns [] = 1
prodRecAns (x:xs) = x*prodRecAns xs

-- Q4
inRangeRecAns :: Int -> Int -> [Int] -> [Int]
inRangeRecAns lo hi [] = []
inRangeRecAns lo hi (x:xs) | x >= lo && x <= hi = x : inRangeRecAns lo hi xs
                           | otherwise          = inRangeRecAns lo hi xs

-- Q5
countEvenRecAns :: [Int] -> Int
countEvenRecAns [] = 0
countEvenRecAns (x:xs) | mod x 2 == 0  = 1 + countEvenRecAns xs
                       | otherwise     = countEvenRecAns xs

-- chapter4
-- Q2
doubleSqr :: Int -> Int
doubleSqr x = (x*2)^2

mapDouSqrAns :: [Int] -> [Int]
mapDouSqrAns xs = map doubleSqr xs

-- Q3
isSameStringAns :: String -> String -> Bool
isSameStringAns xs ys = map toLower xs == map toLower ys

-- Q4
multiplesOf5 :: Int -> Bool
multiplesOf5 x = mod x 5 == 0

filterMod5Ans :: [Int] -> [Int]
filterMod5Ans xs = filter multiplesOf5 xs

-- Q5
minus5 :: Int -> Int
minus5 x = x - 5

prodMinusAns :: [Int] -> Int
prodMinusAns xs = foldr (*) 1 (map minus5 xs)
-- Exercise set 4b: folds

module Set4b where

import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: countNothings with a fold. The function countNothings from
-- the course material can be implemented using foldr. Your task is to
-- define countHelper so that the following definition of countNothings
-- works.
--
-- Hint: You can start by trying to add a type signature for countHelper.
--
-- Challenge: look up the maybe function and use it in countHelper.
--
-- Examples:
--   countNothings []  ==>  0
--   countNothings [Just 1, Nothing, Just 3, Nothing]  ==>  2

countNothings :: [Maybe x] -> Int
countNothings list = foldr (countHelper) (0) (list)

countHelper :: Maybe x -> Int -> Int
countHelper Nothing nothingCounter = nothingCounter + 1
countHelper _ nothingCounter = nothingCounter

------------------------------------------------------------------------------
-- Ex 2: myMaximum with a fold. Just like in the previous exercise,
-- define maxHelper so that the given definition of myMaximum works.
--
-- Examples:
--   myMaximum []  ==>  0
--   myMaximum [1,3,2]  ==>  3

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (current:rest) = foldr (maxHelper) (current) (rest)

maxHelper :: Int -> Int -> Int
maxHelper currentMax current = max (currentMax) (current)

------------------------------------------------------------------------------
-- Ex 3: compute the sum and length of a list with a fold. Define
-- slHelper and slStart so that the given definition of sumAndLength
-- works. This could be used to compute the average of a list.
--
-- Start by giving slStart and slHelper types.
--
-- Examples:
--   sumAndLength []             ==>  (0.0,0)
--   sumAndLength [1.0,2.0,4.0]  ==>  (7.0,3)

sumAndLength :: [Double] -> (Double,Int)
sumAndLength list = foldr (slHelper) (slStart) (list)

slStart :: (Double,Int)
slStart = (0.0, 0)

slHelper :: Double -> (Double, Int) -> (Double, Int)
slHelper current (sum, length) = ((sum + current), (length + 1))

------------------------------------------------------------------------------
-- Ex 4: implement concat with a fold. Define concatHelper and
-- concatStart so that the given definition of myConcat joins inner
-- lists of a list.
--
-- Examples:
--   myConcat [[]]                ==> []
--   myConcat [[1,2,3],[4,5],[6]] ==> [1,2,3,4,5,6]

myConcat :: [[x]] -> [x]
myConcat list = foldr (concatHelper) (concatStart) (list)

concatStart :: [x]
concatStart  = []

concatHelper :: [x] -> [x] -> [x]
concatHelper current concatenatedSoFar = (current ++ concatenatedSoFar)

------------------------------------------------------------------------------
-- Ex 5: get all occurrences of the largest number in a list with a
-- fold. Implement largestHelper so that the given definition of largest works.
--
-- Examples:
--   largest [] ==> []
--   largest [1,3,2] ==> [3]
--   largest [1,3,2,3] ==> [3,3]

largest :: [Int] -> [Int]
largest list = foldr (largestHelper) ([]) (list)

largestHelper :: Int -> [Int] -> [Int]
largestHelper current [] = [current]
largestHelper current (currentLargest:remainingNumbers) = if current > currentLargest then [current] else if current == currentLargest then current : currentLargest : remainingNumbers else currentLargest : remainingNumbers

------------------------------------------------------------------------------
-- Ex 6: get the first element of a list with a fold. Define
-- headHelper so that the given definition of myHead works.
--
-- Start by giving headHelper a type.
--
-- Examples:
--   myHead []  ==>  Nothing
--   myHead [1,2,3]  ==>  Just 1

myHead :: [x] -> Maybe x
myHead list = foldr (headHelper) (Nothing) (list)

headHelper :: x -> Maybe x -> Maybe x
headHelper current _ = Just (current)

------------------------------------------------------------------------------
-- Ex 7: get the last element of a list with a fold. Define lasthelper
-- so that the given definition of myLast works.
--
-- Start by giving lastHelper a type.
--
-- Examples:
--   myLast [] ==> Nothing
--   myLast [1,2,3] ==> Just 3

myLast :: [a] -> Maybe a
myLast list = foldr (lastHelper) (Nothing) (list)

lastHelper :: x -> Maybe x -> Maybe x
lastHelper current Nothing = Just (current)
lastHelper _ current = current

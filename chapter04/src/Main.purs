module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.MonadPlus (guard)

import Data.Array (filter, length, null, (..), (:))
import Data.Array.Unsafe (head, tail)

import Data.Foldable

-- 4.1 (Easy) Write a recursive function which returns true if and only if its
-- input is an even integer.
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

-- 4.2 (Medium) Write a recursive function which counts the number of even
-- integers in an array. Hint: use the head function from Data.Array.Unsafe.
numEven :: Array Int -> Int
numEven arr =
  if null arr
  then 0
  else score (head arr) + numEven (tail arr)
  where
  score :: Int -> Int
  score n = if n `mod` 2 == 0 then 1 else 0


-- 4.3 (Easy) Use the map or <$> function to write a function which calculates
-- the squares of an array of numbers.
square :: Number -> Number
square n = n * n

squareAll :: Array Number -> Array Number
squareAll arr = square <$> arr


-- 4.4 (Easy) Use the filter function to write a function which removes the
-- negative numbers from an array of numbers.
isNonNegative :: Number -> Boolean
isNonNegative n = n >= 0.0

nonNegatives :: Array Number -> Array Number
nonNegatives = filter isNonNegative


-- 4.5 (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to
-- the previous question to use your new operator. Experiment with the precedence
-- level and associativity of your operator in PSCi.
(<$?>) :: forall a. (a -> Boolean) -> Array a -> Array a
(<$?>) = filter

nonNegatives2 :: Array Number -> Array Number
nonNegatives2 arr = isNonNegative <$?> arr


-- 4.6 (Easy) Use the factors function to define a function isPrime which tests
-- if its integer argument is prime or not.
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  return [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1


-- 4.7 (Medium) Write a function which uses do notation to find the cartesian
-- product of two arrays, i.e. the set of all pairs of elements a, b, where a
-- is an element of the first array, and b is an element of the second.
xprod :: Array Int -> Array Int -> Array (Array Int)
xprod xs ys = do
  x <- xs
  y <- ys
  return [x, y]


-- 4.8 (Medium) A Pythagorean triple is an array of numbers [a, b, c] such that
-- a² + b² = c². Use the guard function in an array comprehension to write a
-- function triples which takes a number n and calculates all Pythagorean triples
-- whose components are less than n. Your function should have type Int -> Array
-- (Array Int).
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. (n - 1)
  b <- 1 .. (n - 1)
  c <- 1 .. (n - 1)
  guard $ (a * a) + (b * b) == c * c && a < b && b < c
  return [a, b, c]


-- 4.9 (Difficult) Write a function factorizations which produces all
-- factorizations of an integer n, i.e. arrays of integers whose product is n.
-- Hint: for an integer greater than 1, break the problem down into two
-- subproblems: finding the first factor, and finding the remaining factors.
-- TODO
-- factorizations :: Int -> Array (Array Int)
-- factorizations n =


-- 4.10 (Easy) Use foldl to test whether an array of boolean values are all true.
allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true


-- 4.11 (Medium) Characterize those arrays xs for which the function foldl (==)
-- false xs returns true.
-- What is an allFalse function operating on an array of false values?


-- 4.12 (Medium) Rewrite the following function in tail recursive form using an
-- accumulator parameter:
--
-- import Prelude
-- import Data.Array.Unsafe (head, tail)
--
-- count :: forall a. (a -> Boolean) -> Array a -> Int
-- count _ [] = 0
-- count p xs = if p (head xs)
--              then count p (tail xs) + 1
--              else count p (tail xs)
count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
  count' acc _ [] = acc
  count' acc p xs = if p (head xs)
                    then count' (acc + 1) p (tail xs)
                    else count' acc p (tail xs)


-- 4.13 (Medium) Write reverse in terms of foldl.
reverse2 :: forall a. Array a -> Array a
reverse2 = foldl (\acc x -> x : acc) []


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

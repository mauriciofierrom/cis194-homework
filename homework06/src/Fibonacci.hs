----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 06
--
----------------------------------------------------------------------

module Fibonacci where

import Data.Foldable
----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fib :: Integer -> Integer
fib x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = generate x [0,1]
  where
      generate x' acc
        | length acc ==  fromIntegral x' = last acc
        | otherwise = generate x' (acc ++ [sum . take 2 $ reverse acc])

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = map fib [0..]


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show x = show . take 20 $ streamToList x

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat s = Stream s (streamRepeat s)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream s s') = Stream (f s) (streamMap f s')

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream (f x) (streamFromSeed f (f x))


----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (gPower 6) nats
  where
    gPower :: Integer -> Integer -> Integer
    gPower n x
      | x `mod` 2^n == 0 = n
      | otherwise = gPower (n - 1) x

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a a') (Stream b b') = Stream a (Stream b (interleaveStreams a' b'))

ruler' :: Stream Integer
ruler' = undefined
----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

x :: Stream Integer
x = undefined


----------------------------------------------------------------------
-- Exercise 7 (Optional)
----------------------------------------------------------------------

fib4 :: Integer -> Integer
fib4 = undefined

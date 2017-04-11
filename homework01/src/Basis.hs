----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 01
--
----------------------------------------------------------------------

module Basis where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []

{-toDigits :: Integer -> [Integer]
toDigits x 
  | x <=0 = []
  | otherwise = (getInt . show) x
    where
      getInt :: String -> [Integer]
      getInt = map (read . fromCharToString)
      fromCharToString :: Char -> String
      fromCharToString c = [c]-}

toDigits :: Integer -> [Integer]
toDigits x
  | x <=0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]
----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [x]     = [x]
doubleEveryOther (x:(y:zs)) 
  | (even . length) (x:(y:zs)) = (x * 2) : y : doubleEveryOther zs
  | otherwise = x : (y * 2) : doubleEveryOther zs

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> sumDigits [16,7,12,5]
-- 22

sumDigits :: [Integer] -> Integer
sumDigits (x:xs)
  | null (x:xs) = 0
  | null xs = sum (toDigits x)
  | otherwise = sum (toDigits x) + sumDigits xs
----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |which solves this problem in as few moves as possible.
--
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

-- |
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
-- >>> hanoi 3 "a" "b" "c"
-- [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start finish temp
  | n == 1 = [(start, finish)]
  | otherwise = hanoi (n - 1) start temp finish ++ hanoi 1 start finish temp ++ hanoi (n - 1) temp finish start

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' = undefined

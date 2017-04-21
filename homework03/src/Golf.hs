----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------
module Golf where

import Data.List
----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False],[False]]
-- >>> skips []
-- []

nthElement :: Int -> [a] -> [a]
nthElement n xs = case drop (n-1) xs of
                    []     -> []
                    (x:xs) -> x : nthElement n xs

skips :: [a] -> [[a]]
skips xs = map snd (scanl1 (\x (a,b) -> (a+1, nthElement a b)) (zip [1..(length xs)] (replicate (length xs) xs)))


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = case xs of
                   x:(y:(z:zs))
                     | y > x && y > z -> y:localMaxima (y:(z:zs))
                     | otherwise -> localMaxima (y:(z:zs))
                   [_] -> localMaxima []
                   _ -> []

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"

getBase :: (Enum a, Num a, Ord a) => [a] -> String
getBase xs
  | null $ concat items = []
  | otherwise = getBase (concatMap (drop 1) items) ++ map (\x -> if not $ null x then '*' else ' ') items ++ "\n"
  where 
    items = map (\x -> if elem x . sort $ xs then filter (==x) xs else []) [0..9]

histogram :: [Integer] -> String
histogram xs = getBase  xs ++ "==========\n0123456789\n"

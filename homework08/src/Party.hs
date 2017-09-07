{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 08
--
----------------------------------------------------------------------

module Party where

import Employee
import Data.Tree
import Data.List ( union, unzip )
import Data.Monoid ((<>))

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- instance Monoid GuestList where
--   mempty = GL [] 0
--   mappend gl@(GL _ fun) gl'@(GL _ fun')
--     | fun >= fun' = gl
--     | otherwise = gl'

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emp fun) gl'@(GL emp' fun') = GL interGl funSum
    where
      interGl= emp `union` emp'
      funSum = foldr (\x acc -> acc + empFun x) 0 interGl

glCons :: Employee -> GuestList -> GuestList
glCons Emp{..} (GL eList fun) = GL (Emp empName empFun:eList) (fun + empFun)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl@(GL _ stfu) gl'@(GL _ stfu')
  | stfu >= stfu' = gl
  | otherwise = gl'


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- treeFold :: Monoid b => (a -> b) -> Tree a -> b
-- treeFold :: Monoid b => (a -> b) -> Tree a -> b
-- treeFold f (Node root []) = f root
-- treeFold f (Node root subForest) = f root <> foldMap (treeFold f) subForest

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

treeFold :: Monoid b => (a -> b) -> Tree a -> b
treeFold f (Node root []) = f root
treeFold f (Node root subForest) = f root <> mconcat (map (treeFold f) subForest)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gts = (withBoss, withoutBoss)
  where
    withBoss = glCons emp $ mconcat $ (snd . unzip) gts
    withoutBoss = mconcat $ (fst . unzip) gts

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun (foldIt t)
  where
    foldIt = foldTree nextLevel

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = do
  companyText <- readFile "src/company.txt"
  print $ maxFun (read companyText)

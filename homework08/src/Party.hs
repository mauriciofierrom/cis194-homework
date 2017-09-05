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
import Data.Monoid ((<>))

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

instance Monoid GuestList where
  mempty = GL [] 0
  mappend gl@(GL _ fun) gl'@(GL _ fun')
    | fun >= fun' = gl
    | otherwise = gl'

glCons :: Employee -> GuestList -> GuestList
glCons Emp{..} (GL eList fun) = GL (Emp empName empFun:eList) (fun + empFun)


moreFun :: GuestList -> GuestList -> GuestList
moreFun gl gl' = gl <> gl'


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
treeFold f (Node root subForest) = f root <> mconcat (fmap (treeFold f) subForest)

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel _ gls = (,) (mconcat $ map fst gls) (mconcat $ map snd gls)


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
-- maxFun t = fst (foldIt t) <> snd (foldIt t)
maxFun t = uncurry (<>) (foldIt t)
  where
    foldIt = foldTree nextLevel

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

main :: IO ()
main = undefined

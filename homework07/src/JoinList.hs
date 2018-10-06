----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 07
--
----------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Data.Monoid ((<>))

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag j = case j of
          Empty  -> mempty
          Single m _ -> m
          Append m _ _ -> m


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = undefined

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = undefined

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = undefined


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

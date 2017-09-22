{-# LANGUAGE InstanceSigs #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 10
--
----------------------------------------------------------------------

module AParser where

-- base
import Control.Applicative
import Data.Char


newtype Parser a =
  Parser { runParser :: String -> Maybe (a, String) }


-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f []          = Nothing
    f (x:xs)
      | p x       = Just (x, xs)
      | otherwise = Nothing


-- |
--
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

char :: Char -> Parser Char
char c = satisfy (== c)


posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs


----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser m
    where
      m s = first f <$> runParser p s


first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\_ -> Just (x,[]))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p p' = Parser m
    where
      m s = case runParser p s of
              Nothing -> Nothing
              Just (f, c) -> first f <$> runParser p' c

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'


-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'


-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
intPair =  (\int _ int' -> [int,int']) <$> posInt <*> char ' ' <*> posInt


----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p) (Parser p') = Parser $ \s -> p s <|> p' s



----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((),"abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((),"YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing

intUnit :: Parser ()
intUnit = (\_ -> ()) <$> posInt

upperUnit :: Parser ()
upperUnit = (\_ -> ()) <$> satisfy isUpper

intOrUppercase :: Parser ()
intOrUppercase = intUnit <|> upperUnit

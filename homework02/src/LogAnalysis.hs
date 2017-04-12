{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"
parseMessage :: String -> LogMessage 
parseMessage x = case words x of 
                   ("I":t':m)        -> LogMessage Info (read t') (unwords  m)
                   ("W":t':m)        -> LogMessage Warning (read t') (unwords  m)
                   ("E":n:t':m)      -> LogMessage (Error (read n)) (read t') (unwords m)
                   _                 -> Unknown "This is not in the right format"

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)
----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |MessageTree
--
-- >>> insert (LogMessage Info 2 "Message 2") Leaf
-- Node Leaf (LogMessage Info 2 "Message 2") Leaf
-- >>> insert (LogMessage (Error 4) 3 "Message 3") (Node Leaf (LogMessage Info 2 "Message 2") Leaf)
-- Node Leaf (LogMessage Info 2 "Message 2") (Node Leaf (LogMessage (Error 4) 3 "Message 3") Leaf)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert l Leaf = Node Leaf l Leaf
insert l'@(LogMessage _ t _) (Node l p@(LogMessage _ t' _) r) 
  | t < t'  = Node (insert l' l) p r
  | t >= t' = Node l p (insert l' r)
insert _  (Node _ _ _) = error "Incorrect Format"

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--
build :: [LogMessage] -> MessageTree
build []         = Leaf
-- build (x:[])     = insert x (build [])
build (x:y)      = insert x (build y)
-- build (x:(y:zs)) = insert x (build (y:zs))

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r
-- inOrder (Node Leaf ms Leaf) = [ms]
-- inOrder (Node Leaf ms r) = ms:inOrder(r) 
-- inOrder (Node (Node Leaf m r) ms r') = [m] ++ inOrder(r) ++ [ms] ++ inOrder(r')
-- inOrder (Node (Node l m r) ms r') = inOrder(l) ++ [m] ++ inOrder(r)++[ms] ++ inOrder(r')
-- inOrder n@(Node ln@(Node l m r) ms rn@(Node l' m' r')) 

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--MessageTree
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = toString(filter errorHigherThan x)
  where 
    errorHigherThan (LogMessage (Error t)  _ _) 
      | t > 50 = True
      | otherwise = False
    errorHigherThan (LogMessage _ _ _)    = False
    errorHigherThan (Unknown _)              = False

    toString []                    = []
    toString ((Unknown _):_)       = []
    toString ((LogMessage _ _ s):[]) = [s]
    toString ((LogMessage _ _ s):xs) = s:toString(xs)

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = "Something"

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

getTriples :: [a] -> [[a]]
getTriples (x:[]) = [[x]]
getTriples (x:(y:[])) = [(x:(y:[]))]
getTriples xs 
  | null xs = []
  | otherwise = (take 3) xs : getTriples (drop 3 xs)

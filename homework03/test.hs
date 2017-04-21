import Data.List

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

data Type = Electric
          | Grass
          | Poison
          | Psychic
          | Rock
          | Ground
          deriving Show

data Pokemon = Pokemon Int String Type
              deriving Show

getType :: Pokemon -> Type
getType (Pokemon _ _ t) = t

helperFunc :: Int -> [a] -> [a]
helperFunc _ [] = []

helperFunc n (x:xs)
  | null xs = []
  | n > length (x:xs) = []
  | otherwise = head (drop n (x:xs)) : helperFunc (n+1) xs

-- getBase :: (Enum a, Num a, Ord a) => [a] -> [[a]]
getBase xs
  | null $ concat items = []
  | otherwise = (getBase (concat ((map (drop 1) items)))) ++ "\n" ++ map (\x -> if not $ null x then '*' else ' ') items
  where 
    items = map (\x -> if elem x . sort $ xs then (filter (==x) xs) else []) [0..9]

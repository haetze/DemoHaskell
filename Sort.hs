--Richard Stewing
--19.04.2016

--my Sort function



module Sort where

--import Data.List

data Sorted a = Sorted (Sorted a) (Sorted a) (Sorted a) |
                Single a |
                None
                deriving(Show, Read, Eq)


insert::(Ord a) =>  a -> Sorted a -> Sorted a
insert x (Single y) | x <  y = Sorted (Single x) None (Single y)
                    | y <= x = Sorted (Single y) None (Single x)
insert x None                = Single x
insert x (Sorted (Single y) None (Single z)) | x < y =
                                               Sorted (Single x) (Single y)
                                                      (Single z)
                                             | z < x =
                                               Sorted (Single y) (Single z)
                                                      (Single x)
                                             | otherwise =
                                               Sorted (Single y) (Single x)
                                                      (Single z)

insert x (Sorted a (Single y) b) | x < y     = Sorted (insert x a) (Single y) b
                                 | otherwise = Sorted a (Single y) (insert x b)

sorter:: Ord a => [a] -> Sorted a
sorter [] = None
sorter (x:xs) = insert x (sorter xs)

sortedToList:: Sorted a -> [a]
sortedToList None = []
sortedToList (Single x) = [x]
sortedToList (Sorted a b c) = sortedToList a ++ sortedToList b ++
                              sortedToList c



insertSorted x [] = [x]
insertSorted x (y:[]) | x < y = (x:y:[])
                      | otherwise = (y:x:[])
insertSorted x (y:ys) | x < y = (x:y:ys)
                      | otherwise = (y : insertSorted x ys)
                        

mySort [] = []
mySort (x:[]) = [x]
mySort (x:y:xs) = insertSorted x $ mySort (y:xs)


qsort [] = []
qsort (x:[]) = [x]
qsort xs = qsort (filter f xs) ++ qsort (filter (\s -> not (f s)) xs)
  where
    f s =  s <= sum xs `div` length xs





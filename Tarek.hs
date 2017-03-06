module Tarek where


f::[String] -> [String]
f = foldl (\xs x -> xs ++ [x,x]) [] 

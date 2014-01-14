--Concat.hs
Concat  :: [[a]] -> [a]
Concat = foldr (++) []


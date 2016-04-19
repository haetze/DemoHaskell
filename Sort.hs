--Richard Stewing
--19.04.2016

--my Sort function



module Sort where


insertSorted x [] = [x]
insertSorted x (y:[]) | x < y = (x:y:[])
                      | otherwise = (y:x:[])
insertSorted x (y:ys) | x < y = (x:y:ys)
                      | otherwise = (y : insertSorted x ys)
                        

mySort [] = []
mySort (x:[]) = [x]
mySort (x:y:xs) = insertSorted x $ mySort (y:xs)

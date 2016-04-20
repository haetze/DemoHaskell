--Richard Stewing

--20.04.2016


module SumOfSq where

sumOfSq = sum . map (\x -> x*x)


sumOfSqWithUpperBound x = sumOfSq [1..x]

sumOfSqWithLowerAndUpperBounds x y = sumOfSq [x..y]

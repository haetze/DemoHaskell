--demo1.hs
--
--Richard Stewing
--
--4.8.2014
--

import System.Environment(getArgs)
import Data.List(sort)


biggest xs = last (sort xs)

main = do
	args <- getArgs
	putStrLn (show (biggest (map read args :: [Int])))





--fib.hs
--9.1.2014

import System.Environment(getArgs)

--fibStart:: Integer -> Integer
fibStart  n = fib n 0 1 0
--fib:: Int -> Int -> Int -> Int -> Int
fib 0 _ _ _ = 0
fib x a b n | n<x = fib x b (a+b) (n+1)
fib x a b n | n==x = a


main = do 
	 args <- getArgs
	 let g = read . head $ args
	 let x = map fibStart [1..g]
	 let (y:ys) = x
	 putStrLn $ show y
	 putStrLn . show . head . reverse $ ys
         --putStrLn (show (fibStart (read (head args))))


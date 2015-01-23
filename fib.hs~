--fib.hs
--9.1.2014

import System.Environment(getArgs)

fibStart:: Int -> Int
fibStart  n = fib n 0 1 0
fib:: Int -> Int -> Int -> Int -> Int
fib 0 _ _ _ = 0
fib x a b n | n<x = fib x b (a+b) (n+1)
fib x a b n | n==x = a

main = do 
	 args <- getArgs
         putStrLn (show (fibStart (read (head args))))


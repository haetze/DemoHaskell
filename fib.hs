--fib.hs
--9.1.2014

import System.Environment(getArgs)

--fibStart:: Integer -> Integer
fibStart  n = fib n 0 1 0

fib 0 _ _ _ = 0
fib x a b n | n<x = fib x b (a+b) (n+1)
fib x a b n | n==x = a


fib' n = f !! n
  where f = 0:1: zipWith (+) f (tail f) 

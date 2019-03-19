module Dyn where

fib
  :: Integer
  -> Integer
fib 0 = 0
fib 1 = 1
fib n = a + b
  where
    a = fib $ n-1
    b = fib $ n-2


fibSeq :: [Integer]
fibSeq = 0 : 1 : zipWith (+) fibSeq (tail fibSeq)



module Collatz where

import Data.Array

mkArray :: Ix a => (a,a) -> (a -> b) -> Array a b
mkArray (a,b) f = array (a,b) [(x,f x) | x <- range (a,b)]

col n | even n = n `div` 2
      | odd  n = 3*n + 1


seqForN 1 = [1]
seqForN n = n : seqForN (col n)


f n = a ! n
  where a = array (0,100*n) $ (1,[1]) : [(i, i:r) | i <- [0..100*n]
                                                  ,let c = col i
                                                  ,let r = case c of
                                                         1 -> [1]
                                                         n -> a!n
                                                  ]

f' n = x ! n
  where
    x = mkArray (0,n*100) g
    g 1 = [1]
    g n = n: x ! col n
    

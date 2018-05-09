--Concat.hs
module Concat where

power a b = loop1 a b 1
  where
    loop1 a b c = if c >= 1 then loop1 a (b-1) (c*b) else c

s [] = Nothing
s (x:xs) = Just x

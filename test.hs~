#! /usr/bin/env runhugs +l
--
-- test.hs
-- Copyright (C) 2015 haetze <haetze@localhost>
--
-- Distributed under terms of the MIT license.
--

newtype M a = M a
	deriving (Show, Read)


f :: Fractional b => b->b 
f c = c+0.5

g :: Num a => M a -> a
g (M v) = v


h:: Fractional a => M a -> a
h a = s . f $ g a

s:: Fractional a => a -> a
s a = 1+a

j:: String -> Int
j = read

k:: String -> M Int
k = read

l:: String -> Bool
l ('M':xs) = True && l xs
l ('m':xs) = True && l xs
l []	   = True
l _ 	   = False

a:: Char -> String -> Bool
a c (x:xs) | c /= x = True && a c xs
	   | c == x = True 
a _ [] 	   = False 

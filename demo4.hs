#! /usr/bin/env runhugs +l
--
-- demo4.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Demo4(
	doppMapPar,
	inc,
	dec,
	fib2)
	 where


import Control.Parallel.Strategies


doppMap:: (a->b) -> (b->c) -> [a] -> [c]
doppMap _ _ [] = []
doppMap f1 f2 (x:xs) = ((f2 $ f1 x): doppMap f1 f2 xs)

inc:: Num a => a -> a
inc x = x+1

dec:: Num a => a -> a
dec x = x-1

doppMapPar:: (a->b) -> (b->c) -> [a] -> [c]
doppMapPar f1 f2 xs = doppMap f1 f2 xs `using` parList (rparWith rpar)


fib2:: Integer -> Integer
fib2 a = fibber 0 1 (a-1)

fibber:: Integer -> Integer -> Integer -> Integer
fibber a b 0 = a + b
fibber a b n = fibber b (a+b) (n-1)



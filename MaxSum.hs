#! /usr/bin/env runhugs +l
--
-- MaxSum.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module MaxSum where


smallList [] = []
smallList (0:xs) = smallList xs
smallList (x:[]) = x:[]
smallList (x:y:xs) | x < 0 && y < 0 = smallList (x+y:xs)
		   | x > 0 && y > 0 = smallList (x+y:xs)
		   | otherwise 	    = x : smallList (y:xs)

dropNeg [] = []
dropNeg (x:xs) | x < 0 = xs
	       | otherwise = (x:xs)

merge [] = 0
merge (x:[]) = x
merge (x:y:xs) | -y < sum (y:xs) && x > -y = merge (x+y:xs)
	       |  x < merge xs 	 = merge xs
	       | otherwise	 = x

findBiggestSum = merge . dropNeg . smallList 

#! /usr/bin/env runhugs +l
--
-- Fib.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module Fib where

import Data.Maybe


fib:: [Integer] -> Int -> (Int, [Integer])
fib (x:xs) n | n < length (x:xs) = (y,(x:xs))
	     | n == length (x:xs) = (1, (x:xs))
	     | otherwise = fib (s:x:xs) n
	where	
		y = (length xs) - n + 2
		s = x + head xs


fibs = 1:1: (zipWith (+) fibs $ tail fibs)

get:: [a] -> Int -> Maybe a
get _ 0 = Nothing
get [] _ = Nothing
get (x:xs) 1 = Just x
get (x:xs) n | n > 0 = get xs (n-1)
	     | n < 0 = Nothing







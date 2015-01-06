#! /usr/bin/env runhugs +l
--
-- demo2.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--
--
--
--
--
(<<>>) :: Fractional a => a  -> (a -> a)-> a 
a <<>> f = f a


test4 :: Num a => a-> a 
test4 a = a * 12

test ::   Num a => a -> a 
test a = a+1

test2 :: Fractional a => a -> a 
test2 a = a/2

test3 :: Fractional a => a -> a
test3 a = test a
	<<>> test2


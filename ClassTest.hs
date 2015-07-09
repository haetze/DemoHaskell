#! /usr/bin/env runhugs +l
--
-- ClassTest.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module ClassTest where

data T = T Int 
	| T2 Double
	deriving(Show, Read, Eq)

data Z = Z String

class Test a where
	test:: a -> a -> a

instance Test T where
	test (T x) (T y) = T (x+y)
	test (T2 x) _	 = T2 x
	test (T x)  _    = T x

instance Test Z where
	test (Z a) (Z b) = Z $ a++b
	test (Z a) (T x) = Z $ a ++ show x


f = test (Z "q") (T 1)



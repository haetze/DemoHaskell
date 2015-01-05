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
(<<>>) :: Maybe2 Int  -> (Int -> Maybe2 Int)-> Maybe2 Int
Just2 a <<>> f = if a == 1
	then f 0
	else f a
Nothing2 <<>> _ = Nothing2

data Maybe2 a = Just2 a | Nothing2
	deriving (Show)


test ::   Int  -> Maybe2 Int 
test a = Just2 (a+1)

test2 :: Int -> Maybe2 Int 
test2 a = Just2 (a+2)

test3 :: Int -> Maybe2 Int
test3 a = test a
	<<>> test2


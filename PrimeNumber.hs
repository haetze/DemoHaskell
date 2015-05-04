#! /usr/bin/env runhugs +l
--
-- PrimeNumber.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module PrimeNumber where


type PrimeNumbers = [Integer]

checkIfPrimeNumber :: Integer -> PrimeNumbers -> Bool
checkIfPrimeNumber _ []		= False
checkIfPrimeNumber a (x:xs) | a > x = checkIfPrimeNumber a $ adder a (x:xs)
			    | otherwise = isMemeber (x:xs) a

isMemeber::Eq a => [a] -> a -> Bool
isMemeber [] _ = False
isMemeber (x:xs) a | x == a = True
		   | otherwise = isMemeber xs a

numToBool::(Eq a, Num a) => a -> Bool
numToBool 0 = False
numToBool _ = True

check:: [Bool] -> Bool
check [] = True
check (False:_) = False
check (_:xs) 	= check xs

f:: Integer -> Integer -> Integer
f x y = x `mod` y

adder:: Integer -> PrimeNumbers -> PrimeNumbers
adder a [] = adder a [3,2]
adder a (x:xs) | a < x = (x:xs)
	       | otherwise = adder a $ nextPrime (x:xs)

nextPrime:: PrimeNumbers -> PrimeNumbers
nextPrime (x:xs) = y (x+1) (x:xs)

y:: Integer -> PrimeNumbers -> PrimeNumbers
y a (x:xs) | z a (x:xs) = (a:x:xs)
	   | otherwise 			 = y (a+1) (x:xs)
	where 
	z a x = check . (map numToBool) $ map (f a) x



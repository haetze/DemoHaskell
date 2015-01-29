#! /usr/bin/env runhugs +l
--
-- demo2.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

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

test5 :: [a] -> [a]
test5 (x:xs) = return x

test8 :: [a] -> [a]
test8 (x:xs) = return x ++ test5 xs

test6 :: Num a => a -> [a]
test6 a = return (a+1)

test7 (x:xs) = test8 (x:xs) 
	>>= test6

wordCount = print . length . words =<< getLine

test9:: Int -> Int 
test9  = test . test . test  

data StateString a = State (String, a)
	deriving(Show, Eq)

instance Monad (StateString) where
	State (a, b) >>= f = f b 
	return a = State ("", a)

incState :: Num a => a -> StateString a
incState a = return (a+1)

changeState:: StateString a -> String -> StateString a
changeState (State ("", a) ) s = State (s, a)
changeState (State (b, a)) s = State (s, a)

f1 :: StateString Int  -> StateString Int
f1 (State (a, b)) = inc $  State (a, b)
	>>= dec 

inc :: StateString  Int -> StateString Int 
inc ( State (a, b)) = State (a, b+1)

dec :: Int  -> StateString Int
dec b = State ("", b-1)

add :: Fractional a => a -> a -> a 
add a b = a+b

words2:: String -> [String]
words2 s = wordWork [] "" s


wordWork:: [String] -> String -> String -> [String]
wordWork [] s (' ':ys) | s /= "" = wordWork [s] "" ys
wordWork xs "" (' ':ys) = wordWork xs "" ys
wordWork [] s (y:ys) | y == ' '=wordWork [] s ys 
		     | otherwise = wordWork [] (s++[y]) ys
wordWork (x:xs) s  (' ':ys) = wordWork ((x:xs)++ [s] ) "" ys
wordWork (x:xs) s (y:ys) = wordWork (x:xs) (s++[y]) ys
wordWork (x:xs) "" "" = (x:xs)
wordWork (x:xs) s  "" = (x:xs)++[s]

pow:: Num a=> a -> Int -> a
pow a 1 = a
pow a d = a* (pow a $ d-1)

pow2:: Num a=> a-> Int -> a
pow2 a b = a^b

class Struc a where
	get:: a -> [a] 

class Double2 a where
	double:: a -> a

instance Num a => Double2 a where
	double a = a*a

instance Double2 String where
	double a = a++a

instance  Struc a where 
	get a = [a]

--instance Struc Char where
	--get a = [a]

newShow:: Struc a => a -> [a]
newShow = get

first:: [a] -> a
first (a:as) = a

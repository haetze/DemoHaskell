#! /usr/bin/env runhugs +l
--
-- demo2.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Char

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

manState:: StateString a -> (a -> StateString a) -> StateString a
manState s@(State (state, a)) f = changeState state $ s >>= f 

incState :: Num a => a -> StateString a
incState a =  return (a+1) 

changeState:: String -> StateString a -> StateString a
changeState s (State (b, a)) = State (s++b, a)

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

instance Double2 String where
	double a = a++a

instance Double2 Int where
	double a = a*2

instance Double2 Double where
	double a = a*2


instance  Struc a where 
	get a = [a]

st::(Num a, Struc a)=> a -> [a]
st = get

newShow:: Struc a => a -> [a]
newShow = get

first:: [a] -> a
first (a:as) = a

last2:: [a] -> a
last2 [] = error  "error" 
last2 (a:[]) = a
last2 (a:as) = last as

sum2::Num a =>  [a] -> a
sum2 [] = 0
sum2 (x:xs) = x+ sum2 xs

charSum:: String -> Int
charSum s = sum2 $ fmap ord s

zz:: Num a=> a-> a-> a-> a
zz a b c = a+b+c

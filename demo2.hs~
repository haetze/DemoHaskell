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

--data State = (String, Int)
--f (State (x, z)) = x
f1 :: StateString Int  -> StateString Int
f1 (State (a, b)) = inc $  State (a, b)
	>>= dec 

inc :: StateString  Int -> StateString Int 
inc ( State (a, b)) = State (a, b+1)

dec :: Int  -> StateString Int
dec b = State ("", b-1)

add :: Fractional a => a -> a -> a 
add a b = a+b

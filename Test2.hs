#! /usr/bin/env runhugs +l
--
-- Test.hs
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE GADTs  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Test where


data T a where
	T1 :: Bool -> T Bool
	T2 :: T a

f :: T a -> a -> a
f x y = case x of
	T1 z -> z
	T2   -> y


fac :: (Ord a, Num a) => a -> a
fac x | x <= 1 = 1
      | otherwise = x * (fac $ x-1)

fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

fib2 0 = 0
fib2 a = fibber a 0 1
	where 
		fibber 0 x y = x
		fibber n x y = fibber (n-1) y (x+y)

grow f x = (f x) : x

ff:: Num a => [a] -> a
ff = sum

loop:: Int -> a -> (a->a) -> a
loop 0 x _ = x
loop n x f = loop (n-1) y f
	where 
		y = f x

doublehead (x:xs) = x*2

check x = x == reverse x


sum1 (x:xs) = foldr (+) x xs

t 0 = 0
t 1 = 1
t n = n - 1



class Show a => Tests a where 
	m :: a -> String

instance (Show a, Num a) => Tests a where
	m x = show x ++ show x



fa:: Tests a => a -> String
fa a = m a ++ "11"



data List a = Element a (List a) 
	| End
	deriving(Show, Read)


listFromList [] = End
listFromList (x:xs) = Element x $ listFromList xs

list2List End = []
list2List (Element x End) = [x]
list2List (Element x xs) = (x:list2List xs)

reverser x = listFromList . reverse $ list2List x

atFront x xs = Element x xs
append x xs = reverser . atFront x $ reverser xs 


listLength:: List a -> Integer
listLength End = 0
listLength (Element _ x) = 1 + listLength x


data JSObject = JSObject [(String, String)]
	deriving(Show, Eq)


splitToNameAndSt (x:xs) = (a,b) 
	where 
		a = findWordTo (x:xs) 
		b = drop2 xs $ length a


drop2 xs 0     = xs
drop2 (x:xs) n = drop2 xs (n-1)


splitBy _ [] = []
splitBy y (x:xs) = splitter y (x:xs) [] []

splitter y (x:xs) n m | y == x = splitter y xs (n ++ m:[]) [] 
		      |otherwise = splitter y xs n (m ++ x:[])
splitter y [] n m = n ++ m:[]


fixList [] = []
fixList (x:[]) = x:[] 
fixList (x:y:[]) | last y == '"'  && head y /= '"' = (x++","++y):[]
		 | otherwise = x:y:[] 
fixList (x:y:xs) | last y == '"' && head y /= '"'= fixList ((x++","++y):xs)
		 | otherwise = x:fixList (y:xs)

fixer xs = loop (length xs) xs fixList


findWordTo (':':x) = ""
findWordTo (x:':':_) = x:[]
findWordTo (x:xs) = (x:findWordTo xs)


string2JSON x = JSObject . map unrapper . map splitToNameAndSt . fixer $ splitBy ',' z
	where 
		y = tail x
		z = reverse . tail $ reverse y


unrapper (x, y) | head x == '"' && last x == '"' && head y == '"' && last y == '"'= (a, b) 
		| head x == '"' && last x == '"' = (a, y)
		| head y == '"' && last y == '"' = (x, b) 
		| otherwise = (x, y)
	where 
		a1 = tail x
		a = reverse . tail $ reverse a1
		b1 = tail y 
		b = reverse . tail $ reverse b1




createPairString (x,y) = show x ++":"++ show y


createJSONString x 	=  "{" ++ f x
	where 
		f []     = "{}"
		f (x:[]) = x ++ "}"
		f (x:xs) = x ++ "," ++ f xs

find ::(Eq a ) =>  a -> [a] -> Maybe Int
find x (z:[]) | x /= z = Nothing
find x (z:zs) | z == x = Just 0
	| otherwise = (Just 1) + find x zs

between _ _ [] 		    = Nothing
between z x (y:ys) | z == y = find z (y:ys) >> find x (y:ys) >> Just ( z: keep x ys)
		   | otherwise = find z (y:ys) >> find x (y:ys) >> between z x ys 
	where	
		keep x (y:ys) | x == y = x:[]
			|otherwise     = y: keep x ys
		keep _ [] 	       = []


instance Num a => Num (Maybe a) where
	Just x + Just y = Just $ x + y
	_      + Nothing = Nothing
	Nothing + _ 	= Nothing




between2 x y zs = findF x zs >>= findS y zs >>= compose
	where
		findF x zs = find x zs
		findS y zs i = find y (drop i zs) >>= r i
		r i n = Just (i, n)
		compose (i, n) = Just . reverse . drop n . reverse $ drop i zs 





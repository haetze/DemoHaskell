#! /usr/bin/env runhugs +l
--
-- PerMult.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module PerMult where
import Data.List
import Control.Parallel
import Control.Parallel.Strategies

data El a = El a Bool
	deriving(Show, Read, Eq, Ord)


createList::[a]->[El a]
createList [] 	= []
createList (x:xs) = (El x False): createList xs

getX::El a -> a
getX (El x _) = x

getBool:: El a -> Bool 
getBool (El _ b) = b

createMult:: [[a]] -> [[El a]]
createMult =  map createList

changeStatus::El a -> El a
changeStatus (El x b) = El x (not b)

searchNextElement::Eq a=> [El a] -> El a -> (Maybe (El a), [El a])
searchNextElement [] _ = (Nothing, [])
searchNextElement (x:xs) y = s (x:xs) x y
	where 
	s::Eq a => [El a] -> El a->El a-> (Maybe (El a), [El a])
	s (x:[]) v y | getX x == getX y && not (getBool x)= (Just v, x2 :[])
		     | otherwise = (Nothing, x:[])
		where
		x2 = changeStatus x
	s (x:xs) v y |getX x==getX y&&not(getBool x)=(Just(head xs), x2:xs) 
			   | otherwise = (j , is)
		where
		(j, i) = s xs v y
		is = x:i
		x2 = changeStatus x
		
findReal::Eq a => [[El a]] -> El a-> (Maybe (El a), [[El a]])
findReal [] _ = (Nothing, [])
findReal (x:[]) y = (c, [i])
	where 
	(c, i) = searchNextElement x y
findReal (x:xs) y = case c of
	Nothing -> (k, l)
	Just v -> (o, p) 
		where 
		(o2, p1) = findReal xs v 
		o = case o2 of
			Nothing -> Just  v
			Just x -> Just  x
		p = i:p1
		(c, i) = searchNextElement x y
	where
	(c, i) = searchNextElement x y
	(k, l1) = findReal xs y
	l = x:l1

findCycle::Eq a=>[[El a]]->El a-> ([El a], [[El a]])
findCycle xs y = ff xs y [y]
	where
	ff::Eq a=>[[El a]]->El a->[El a]->([El a], [[El a]])
	ff xs y ys = do
		let (x, u) = findReal xs y
		case x of 
			Nothing -> (ys, xs)
			Just v  -> if getX v == getX (head ys) then
				(ys, xs)
				else ff u v (ys ++[v])


pickNext::[El a] -> Maybe (El a)
pickNext [] = Nothing
pickNext (x:xs) | not $ getBool x = Just x
		| otherwise 	= pickNext xs

cycle2::(Ord a,Eq a)=> [[El a]] -> [[El a]]
cycle2 []     = []
cycle2 (x:xs) = reverse. sort . filterDup  $ (map (putSmallestFirst . dropFirst .findCycle (x:xs)) x `using` parList (rparWith rpar))
	


dropFirst::(a,b) -> a
dropFirst (x,y) = x	

rot::[a] -> [a]
rot [] = []
rot (x:xs) = xs++[x]

rotTil::Eq a => [a] ->  a -> [a]
rotTil [] _ 	       = []
rotTil (x:xs) y | x==y = (x:xs)
		| elem y (x:xs) = rot (x:xs) `rotTil` y
		| otherwise = error "List does not contain element" 

putSmallestFirst::(Ord a, Eq a) => [a] -> [a]
putSmallestFirst x = rotTil x . head $ sort x


filterDup::(Eq a) => [a] -> [a]
filterDup [] = []
filterDup x = f x []
	where
	f::Eq a=> [a] -> [a] -> [a]
	f (x:xs) ys | not $ elem x ys = f xs (ys++[x])
		    | otherwise       = f xs ys
	f [] 	 ys 		      = ys


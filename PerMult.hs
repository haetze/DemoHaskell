#! /usr/bin/env runhugs +l
--
-- PerMult.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module PerMult where


data El a = El a a Bool
	deriving(Show, Read, Eq)


createList::[a]->[El a]
createList (x:xs) = (El x (head xs) False): c x xs
	where	
	c _ [] = []
	c x (y:[]) = [El y x False]
	c x (y:ys) = El y (head ys) False : c x ys

getX::El a -> a
getX (El x _ _) = x

getY:: El a -> a
getY (El _ y _) = y

getBool:: El a -> Bool 
getBool (El _ _ b) = b

createMult:: [[a]] -> [El a]
createMult = concat . map createList

changeStatus::El a -> El a
changeStatus (El x y b) = El x y (not b)

swap:: El a -> El a
swap (El x y b) = El y x b	


findStart::[El a] -> El a
findStart (x:xs) | not $ getBool x = x
		| otherwise	  = findStart xs



findNext::Eq a => [El a]->[El a] -> El a -> (El a, [El a], [El a]) 
findNext [] _ 	x = (x, [],[])
findNext (x:xs) ys y | getX x == getY y && not (getBool x) = (z, ys2 , xs)
		  | getX x == getX y && getY y == getY x = (t1, uu, t3)
		  | otherwise = (t1, t2,t3)
	where
	z = changeStatus x
	(t1, t2, t3) = findNext xs ys y
	o = reverse ys
	p = reverse t2
	p2 = drop ((length xs)+1) p	
	o2 = drop ((length xs)+1) o
	ys2 = reverse $ reverse xs ++ [z] ++ o2
	uu = reverse $ reverse t3 ++ [t1] ++ [z] ++ p2

findReal::Eq a => [El a] -> [El a] -> El a -> (El a, [El a], [El a])
findReal xs ys x = case w == x of
	True -> (x, ys, xs)
	False -> findReal xs2 ys2 w
	where
	(e1, e2, e3) = findNext xs ys x
	(f1,f2,f3) = findNextX e3 e2 e1
	(w, ys2, xs2) = (f1, f2, f3)
	

eq::Eq a=> El a -> El a -> Bool
eq x y = getX x == getX y && getY x == getY y

proc:: Eq a=> [El a] -> El a -> Bool
proc [] _ = not False
proc (x:xs) n | getX x == getX n = False
	      | otherwise = proc xs n 

findNextX::Eq a=> [El a] -> [El a] -> El a-> (El a, [El a],[El a])
findNextX xs ys x = case d == swap x of
	True-> (x, ys, xs)
	False-> (d, m, k)
	where
	(d, m, k) = findNext xs ys (swap x)

--last = head . reverse




cycle2::Eq a=> [El a] -> [El a] -> [El a] -> ([El a], [El a], [El a]) 
cycle2 xs ys cs = case ds == cs of
	True -> (cs, ys, xs)
	False -> cycle2 ys2 ys2 ds
	where
	(e1, ys2, xs2) = findReal xs ys (last cs)
	ds = case e1 == last cs of
		True -> cs
		False -> cs ++ [e1]

	
 		


	

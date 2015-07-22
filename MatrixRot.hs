#! /usr/bin/env runhugs +l
--
-- MatrixRot.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--
--
--Functions exspect the input Matrix to be well formatted, checkMatrix return True if that is the case and False if not.

module MatrixRot
	where

data Matrix a = Matrix [Row a]
	deriving(Show, Read, Eq)
data Row a = R [a]
	deriving(Show, Read, Eq)
	
data  M a = M [Element a]
	deriving(Show, Read, Eq)
data Element a = E (COO, a)
	deriving(Show, Read, Eq)
type COO = (Int, Int)

data ComputeMatrix a = ComputeMatrix (M a) Int Int

createMatrixFromList::Num a => [[a]] -> Matrix a
createMatrixFromList (x:xs) = Matrix $ map createRowFromList (x:xs)

createRowFromList::Num a => [a] -> Row a
createRowFromList xs = R xs
	
calcNewPos::Num a=> Int -> Element a -> Element a
calcNewPos s (E ((x,y), v)) =  E ((s-y-1, x), v)

rotateMatrix::Num a=> ComputeMatrix a -> ComputeMatrix a
rotateMatrix (ComputeMatrix (M m) s r) = ComputeMatrix (M (map(calcNewPos s) m)) r s 

--The Matrix is returned unmodified in case it is bad formatted
rotateMatrix90Deg::Num a =>  Matrix a -> Matrix a 
rotateMatrix90Deg x =case checkMatrix x of 
	True ->  createMatrix . rotateMatrix $ createComputeMatrix x
	False -> x

checkMatrix::Num a=> Matrix a -> Bool
checkMatrix (Matrix [R []]) = False
checkMatrix (Matrix []) 	 = False
checkMatrix (Matrix ( R x:[])) = True
checkMatrix (Matrix (R x:xs)) = check xs $ length x
	where
	check [] _ = True
	check (R x:xs) n = length x == n && check xs n


createComputeMatrix::Num a =>Matrix a -> ComputeMatrix a
createComputeMatrix (Matrix [])    = ComputeMatrix (M []) 0 0
createComputeMatrix (Matrix [R []]) = createComputeMatrix (Matrix [])
createComputeMatrix (Matrix (R x:xs)) = ComputeMatrix z ( length (R x:xs)) (length x) 
	where
	z = createM (0, 0) (Matrix (R x:xs))

createM::Num a =>  COO -> Matrix a -> M a
createM _	(Matrix [])	= M []
createM (x, y) (Matrix (R []:xs)) = createM (0, y+1) (Matrix xs)
createM (x, y) (Matrix ((R z):xs)) = M $ E ((x, y), (head z)) : a
	where
	M a = createM (x+1, y) (Matrix (R (tail z): xs)) 


createRow::Num a=>ComputeMatrix a -> Int -> Row a
createRow (ComputeMatrix (M x) s r) n = R $ row (ComputeMatrix (M y) 1 r) 0
	where 
	y = filter (f n) x
	f n ( E ((_,y),_)) = n == y
	


row::Num a => ComputeMatrix a -> Int -> [a]
row (ComputeMatrix _ s r) n | n >= r = []
row cm@(ComputeMatrix (M m) s r) n = find (M m) n : row cm (n+1)
	

find::Num a =>  M a -> Int -> a 
find (M []) 	_ 	= 0 
find (M ((E ((x,_),a)):xs)) n | x == n = a
		| otherwise = find (M xs) n 

createMatrix::Num a=> ComputeMatrix a -> Matrix a
createMatrix cm@(ComputeMatrix (M m) s r) = rows2 cm 0
	where
	rows2 x n = Matrix (rows x n) 
	rows _ n | n >= s = []
	rows m n = createRow m n : rows m (n+1)

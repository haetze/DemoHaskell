#! /usr/bin/env runhugs +l
--
-- MatrixRot.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module MatrixRot
	where

type Matrix = [Row]
type Row = [Double]
	
type M = [Element]
type Element = (COO, Double)
type COO = (Int, Int)

data ComputeMatrix = ComputeMatrix M Int Int
	
calcNewPos:: Int -> Element -> Element
calcNewPos s ((x,y), v) = ((s-y-1, x), v)

rotateMatrix:: ComputeMatrix -> ComputeMatrix
rotateMatrix (ComputeMatrix m s r) = ComputeMatrix (map(calcNewPos s) m) s r

rotateMatrix90Deg:: Matrix -> Matrix
rotateMatrix90Deg = createMatrix . rotateMatrix . createComputeMatrix

createComputeMatrix::Matrix -> ComputeMatrix
createComputeMatrix (x:xs) = ComputeMatrix z ( length (x:xs)) (length x) 
	where
	z = createM (0, 0) (x:xs)

createM:: COO -> Matrix -> M
createM _	[]	= []
createM (x, y) ([]:xs) = createM (0, y+1) xs
createM (x, y) (z:xs) = ((x, y), head z) : createM (x+1, y) (tail z: xs)


createRow:: ComputeMatrix -> Int -> [Double]
createRow (ComputeMatrix x s r) n = row (ComputeMatrix y s r) 0
	where 
	y = filter (f n) x
	f n ((_,y),_) = n == y
	


row:: ComputeMatrix -> Int -> [Double]
row (ComputeMatrix _ s r) n | n >= s = []
row cm@(ComputeMatrix m s r) n = find m n : row cm (n+1)
	

find:: M -> Int -> Double
find [] 	_ 	= 0
find (((x,_),a):xs) n | x == n = a
		| otherwise = find xs n 

createMatrix:: ComputeMatrix -> Matrix
createMatrix cm@(ComputeMatrix m s r) = rows cm 0
	where
	rows _ n | n >= r = []
	rows m n = createRow m n : rows m (n+1)

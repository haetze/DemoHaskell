#! /usr/bin/env runhugs +l
--
-- MatrixMulst.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module MatrixMulst where


import MatrixRot

multMatrixWithMatrix:: Matrix -> Matrix -> Matrix
multMatrixWithMatrix m n = createMatrix $ mult j k
	where
	j = createComputeMatrix m
	k = createComputeMatrix n


computeElementFromRows:: Row -> Row -> Double
computeElementFromRows [] [] = 0
computeElementFromRows (x:xs) (y:ys) = x*y+computeElementFromRows xs ys

mult:: ComputeMatrix -> ComputeMatrix -> ComputeMatrix
mult xs@(ComputeMatrix x s r) ys@(ComputeMatrix y s2 r2) | r == s2 && s == r2 = multM xs $ rotateMatrix  ys
		| otherwise = error "Wrong format"

multM:: ComputeMatrix -> ComputeMatrix -> ComputeMatrix
multM a@(ComputeMatrix x s r) b@(ComputeMatrix y s2 r2) = zz
	where 
	e = createMatrix a
	f = createMatrix b
	z = ff e f 
	ff [] _ = []
	ff (x:xs) ys = calcRow x ys : ff xs ys 
	zz = createComputeMatrix z
	
calcRow:: Row -> Matrix -> Row			
calcRow x (y:[]) = computeElementFromRows x (reverse y) : []
calcRow x []     = []
calcRow x (y:ys) = computeElementFromRows x (reverse y) : calcRow x ys




 





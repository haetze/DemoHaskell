#! /usr/bin/env runhugs +l
--
-- MatrixMulst.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module MatrixMulst where


import MatrixRot

multMatrixWithMatrix::Num a=> Matrix a -> Matrix a -> Maybe (Matrix a)
multMatrixWithMatrix m n = case checkMatrix m && checkMatrix n of
	 	True ->	Just . createMatrix $ mult j k
		False -> Nothing
	where
	j = createComputeMatrix m
	k = createComputeMatrix n


computeElementFromRows::Num a=>Row a -> Row a -> a 
computeElementFromRows (R []) (R []) = 0
computeElementFromRows (R (x:xs)) (R (y:ys)) = x*y+computeElementFromRows (R xs) (R ys)

mult::Num a => ComputeMatrix a -> ComputeMatrix a -> ComputeMatrix a
mult xs@(ComputeMatrix x s r) ys@(ComputeMatrix y s2 r2) | r == s2 = multM xs $ rotateMatrix  ys
		| otherwise = error "Wrong format"

multM::Num a=> ComputeMatrix a -> ComputeMatrix a -> ComputeMatrix a
multM a@(ComputeMatrix x s r) b@(ComputeMatrix y s2 r2) = zz
	where 
	Matrix e = createMatrix a
	f = createMatrix b
	z = ff e f 
	ff [] _ = []
	ff (x:xs) ys = calcRow x ys : ff xs ys 
	zz = createComputeMatrix (Matrix z)
	
calcRow::Num a =>  Row a -> Matrix a -> Row a			
calcRow (R x) (Matrix (R y:[])) = R $ computeElementFromRows (R x) (R (reverse y)) : []
calcRow (R x) (Matrix [])     = R []
calcRow (R x) (Matrix (R y:ys)) = R $ computeElementFromRows (R x) (R (reverse y)) : z
	where
	R z =  calcRow (R x) (Matrix ys)





#! /usr/bin/env runhugs +l
--
-- MatrixTest.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module MatrixTest where


import MatrixMulst
import MatrixRot
import Test.QuickCheck

check:: Matrix -> Bool
check x = x == (rotateMatrix90Deg . rotateMatrix90Deg . rotateMatrix90Deg $ rotateMatrix90Deg x)

check2:: Matrix -> Matrix -> Bool
check2 m n = x == y
	where
	j = multMatrixWithMatrix m n
	k = multMatrixWithMatrix n m
	x = getRowNumber j
	y = getCNumber k
	getRowNumber:: Maybe Matrix -> Int
	getRowNumber Nothing = 0
	getRowNumber (Just xs) = length xs
	getCNumber Nothing = 0
	getCNumber (Just (x:xs)) = length x




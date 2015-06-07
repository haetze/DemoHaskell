#! /usr/bin/env runhugs +l
--
-- LambdaSubs.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module LambdaSubs where


import LambdaTypes

shiftFrom (Var x) c d | x < c = Var x
		      | x >= c = Var (x+d)
shiftFrom (Abs x) c d = shiftFrom x (c+1) d
shiftFrom (App x y) c d = App (shiftFrom x c d) (shiftFrom y c d)


sub t1 t2 v | t1 == v = t2
	    | otherwise = v
sub (Var t1) t2 (Abs x) = Abs $ sub (Var (t1+1)) (shiftFrom t2 0 1) x
sub t1 t2 (App x y) = App (sub t1 t2 x) (sub t1 t2 y)



 

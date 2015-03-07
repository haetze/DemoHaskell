#! /usr/bin/env runhugs +l
--
-- Functions.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Functions where


newtype MathFunction a = Func [a]
	deriving(Show, Read)

calcValue::MathFunction Double -> Double -> Double
calcValue (Func (x:[])) _ = x
calcValue (Func (x:xs)) b = (b^length xs)*x + calcValue (Func xs) b


calcDif::(Num a) => MathFunction a -> MathFunction a
calcDif (Func (x:[])) = Func []
calcDif (Func (x:xs)) = Func [z] +++ calcDif (Func xs)
	where 
		z = mult x $ length xs
			
mult::Num a=> a -> Int -> a
mult x 0 = 0
mult x a = x + mult x (a-1)


(+++)::Num a => MathFunction a -> MathFunction a -> MathFunction a
(Func xs) +++ (Func ys) = Func (xs++ys)

multTwoList::Num a => [a] -> [a] -> [[a]]
multTwoList (x:[]) ys = [(map (*x) ys)]
multTwoList (x:xs) ys = [(map (*x) ys)] ++ multTwoList xs ys

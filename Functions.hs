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

multTwoLFunctions::Num a => MathFunction a -> MathFunction a-> MathFunction a
multTwoLFunctions (Func xs) (Func ys) = Func ( shrink $ multTwoList xs ys)

shrink::Num a=> [[a]] -> [a]
shrink (x:xs) = shrink2 1 ((pullTwoTogether  0 x (head xs)) : (tail xs))

shrink2:: Num a=> Int -> [[a]] -> [a]
shrink2 _ (x:[]) = x
shrink2 n (x:xs) =  shrink2 (n+1) ((pullTwoTogether  n x (head xs)) : (tail xs))


pullTwoTogether::Num a=> Int -> [a] -> [a] -> [a]
pullTwoTogether 0 xs ys = ((head xs) : pull (tail xs) ys)
pullTwoTogether n xs ys = (head xs : (pullTwoTogether (n-1) (tail xs) ys))

pull:: Num a => [a] -> [a] -> [a]
pull (x:[]) (y:ys) = (x+y):ys
pull (x:xs) (y:ys) = ((x+y):pull xs ys)


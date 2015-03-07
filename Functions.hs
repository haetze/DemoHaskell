#! /usr/bin/env runhugs +l
--
-- Functions.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Functions where


data MathFunction a = Func [a]
	| EFunc [a] [a]
	| RatFunc [a] [a]
	| LnFunc [a] [a] [a] 
	| Sum [MathFunction a] 
	deriving(Show, Read)

calcValue::MathFunction Double -> Double -> Double
calcValue (Func (x:[])) _ = x
calcValue (Func (x:xs)) b = (b^length xs)*x + calcValue (Func xs) b
calcValue (EFunc xs ys) b = (exp (calcValue (Func xs) b)) * (calcValue (Func ys) b)
calcValue (LnFunc xs ys cs) b = (log $ calcValue (Func xs) b) * (calcValue (Func ys) b) / (calcValue (Func cs) b)
calcValue (RatFunc xs ys) b = (calcValue (Func xs) b) / (calcValue (Func ys) b)
calcValue (Sum xs)	  b = mapCalcValue xs b

mapCalcValue:: [MathFunction Double] -> Double -> Double
mapCalcValue (x:[]) b = calcValue x b
mapCalcValue (x:xs) b = (calcValue x b) + (mapCalcValue xs b)


calcDif::(Num a) => MathFunction a -> MathFunction a
calcDif (Func (x:[])) = Func [0]
calcDif (Func (x:xs)) = Func [z] +++ calcDif (Func xs)
	where 
		z = mult x $ length xs
calcDif (EFunc xs ys) = EFunc xs a
	where 
	Func a = (multTwoLFunctions (calcDif (Func xs)) (Func ys)) `add`  (calcDif (Func ys))
calcDif (LnFunc xs ys cs) = Sum [LnFunc xs c cs, RatFunc a b]
	where 
	Func c = calcDif (Func ys)
	Func a =  (multTwoLFunctions (Func ys) (calcDif (Func xs)))
	b = xs
calcDif (Sum xs) = Sum $ map calcDif xs
calcDif (RatFunc xs ys) = RatFunc a b
	where 
		Func a = (multTwoLFunctions (calcDif (Func xs)) (Func ys)) `sub` (multTwoLFunctions (Func xs) (calcDif (Func ys)))
		Func b = (multTwoLFunctions (Func ys) (Func ys))

sub:: Num a => MathFunction a -> MathFunction a -> MathFunction a
sub (Func xs) (Func ys) = add (Func xs) (Func (map (*(-1)) ys))

add::Num a => MathFunction a-> MathFunction a-> MathFunction a
add (Func xs) (Func ys) = Func $ reverse (adder (reverse xs) (reverse ys))

adder::Num a=> [a] -> [a] -> [a]
adder (x:[]) (y:ys) | length ys /= 0 = (x+y:ys)
adder (x:xs) (y:[]) | length xs /= 0 = (x+y:xs)
adder (x:[]) (y:[]) 		     = (x+y:[])
adder (x:xs) (y:ys) 		     = ((x+y):[]) ++ (adder xs ys)

mult::Num a=> a -> Int -> a
mult x 0 = 0
mult x a = x + mult x (a-1)


(+++)::Num a => MathFunction a -> MathFunction a -> MathFunction a
(Func xs) +++ (Func ys) = Func (xs++ys)

multTwoList::Num a => [a] -> [a] -> [[a]]
multTwoList (x:[]) ys = [(map (*x) ys)]
multTwoList (x:xs) ys = [(map (*x) ys)] ++ multTwoList xs ys

multTwoLFunctions::Num a => MathFunction a -> MathFunction a-> MathFunction a
multTwoLFunctions (Func xs) (Func ys) =  shrink $ multTwoList xs ys

shrink::Num a=> [[a]] -> MathFunction a
shrink (x:[]) = Func x
shrink (x:xs) =Func ( shrink2 1 ((pullTwoTogether  0 x (head xs)) : (tail xs)))

shrink2:: Num a=> Int -> [[a]] -> [a]
shrink2 _ (x:[]) = x
shrink2 n (x:xs) =  shrink2 (n+1) ((pullTwoTogether  n x (head xs)) : (tail xs))


pullTwoTogether::Num a=> Int -> [a] -> [a] -> [a]
pullTwoTogether 0 xs ys = ((head xs) : pull (tail xs) ys)
pullTwoTogether n xs ys = (head xs : (pullTwoTogether (n-1) (tail xs) ys))

pull:: Num a => [a] -> [a] -> [a]
pull [] ys 	   = ys
pull xs [] 	   = xs
pull (x:[]) (y:ys) = (x+y):ys
pull (x:xs) (y:[]) = (x+y):xs
pull (x:xs) (y:ys) = ((x+y):pull xs ys)


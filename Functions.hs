#! /usr/bin/env runhugs +l
--
-- Functions.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--
--


module Functions (
	MathFunction,
	calcValue,
	calcDif,
	killLeadingZeros)
	where


import Control.Parallel
import Control.Parallel.Strategies


data MathFunction a = Func [a]
	| EFunc [a] [a]
	| RatFunc [a] [a]
	| LnFunc [a] [a] [a] 
	| Sum [MathFunction a] 
	deriving(Show, Read)

calcValue::MathFunction Double -> Double -> Double
calcValue (Func (x:[])) _ = x
calcValue (Func (x:xs)) b = sum $ ([(b^length xs)*x] ++ [calcValue (Func xs) b] `using`  parList (rparWith rpar))
calcValue (EFunc xs ys) b = (exp (calcValue (Func xs) b)) * (calcValue (Func ys) b)
calcValue (LnFunc xs ys cs) b = (log $ calcValue (Func xs) b) * (calcValue (Func ys) b) / (calcValue (Func cs) b)
calcValue (RatFunc xs ys) b = (calcValue (Func xs) b) / (calcValue (Func ys) b)
calcValue (Sum xs)	  b = mapCalcValue xs b

mapCalcValue:: [MathFunction Double] -> Double -> Double
mapCalcValue (x:[]) b = calcValue x b
mapCalcValue (x:xs) b = sum $  (([(calcValue x b)] ++ [(mapCalcValue xs b)]) `using` parList (rparWith rpar))


calcDif::(Num a) => MathFunction a -> MathFunction a
calcDif (Func (x:[])) = Func []
calcDif (Func (x:xs)) = fold $ ([Func [z]] ++ [calcDif (Func xs)] `using` parList (rparWith rpar))
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

fold:: Num a=> [MathFunction a] -> MathFunction a
fold (x:[]) = x
fold (x:xs) = fold (s:ss)
	where 
	s = x +++ (head xs)
	ss = tail xs

(+++)::Num a => MathFunction a -> MathFunction a -> MathFunction a
(Func xs) +++ (Func ys) = Func (xs++ys)

multTwoList::Num a => [a] -> [a] -> [[a]]
multTwoList (x:[]) ys = [(map (*x) ys)]
multTwoList (x:xs) ys = [(map (*x) ys)] ++ multTwoList xs ys

--Just for (Func xs), no more complex calc 
multTwoLFunctions::Num a => MathFunction a -> MathFunction a-> MathFunction a
multTwoLFunctions (Func xs) (Func ys) =  shrink $ multTwoList xs ys
multTwoLFunctions (Func xs) (EFunc ys cs) = EFunc ys a
	where 
	Func a = multTwoLFunctions (Func xs) (Func cs)
multTwoLFunctions (Func xs) (LnFunc ys cs vs) = LnFunc ys a vs
	where
	Func a = multTwoLFunctions (Func xs) (Func cs)
multTwoLFunctions (Func xs) (RatFunc ys cs) = RatFunc a cs
	where 
	Func a = multTwoLFunctions (Func xs) (Func ys)
multTwoLFunctions (Func xs) (Sum ys) = Sum $ map (multTwoLFunctions (Func xs)) ys

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

killLeadingZeros::(Eq a, Num a) => MathFunction a -> MathFunction a
killLeadingZeros (Func (0:xs)) = killLeadingZeros (Func xs)
killLeadingZeros (Func (x:xs)) = Func (x:xs)
killLeadingZeros (EFunc xs ys) = EFunc a b
	where 
	Func a = killLeadingZeros (Func xs)
	Func b = killLeadingZeros (Func ys)
killLeadingZeros (LnFunc xs ys cs) = LnFunc a b c
	where
	Func a = killLeadingZeros (Func xs)
	Func b = killLeadingZeros (Func ys)
	Func c = killLeadingZeros (Func cs)
killLeadingZeros (RatFunc xs ys) = RatFunc a b
	where 
	Func a = killLeadingZeros (Func xs)
	Func b = killLeadingZeros (Func ys)
killLeadingZeros (Sum xs) = Sum $ map killLeadingZeros xs 


findX::MathFunction Double -> Double -> Double -> Double -> Double
findX f y x 0 = x
findX f y x n | (calcValue f x) == y = x
	      | (((calcValue f x) - y)) < n && (((calcValue f x) - y) > ((-1)*n)) = x
	      | otherwise 	     = findX f y a n
	where
	 	a = newX f x y

newX:: MathFunction Double -> Double -> Double -> Double
newX f x y = findXForLinear f2 y
	where 
		f2 = Func [ calcValue (calcDif f) x, (calcValue f x) - (calcValue (calcDif f) x)*x]

findXForLinear:: MathFunction Double -> Double -> Double 
findXForLinear (Func (x:xs)) y = (y-(head xs))/x

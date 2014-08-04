--add.hs
--

{-#LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

--add :: Double -> Double -> Double
--add a b = (a)+(b)

sub :: Double -> Double -> Double
sub a b = (a)-(b)

dev :: Double -> Double -> Double
dev a b = (a)/(b)

mult :: Double -> Double -> Double
mult a b = (a)*(b)

times2 :: Double -> Double
times2 a = 2*a

(*+*) :: Double -> Double -> DoubleNGUAGE BangPatterns #-}
b *+* a  = (a+b)*(b+a) 

(+++) :: (Double, Double) -> (Double, Double) -> (Double, Double)
(a, b) +++ (c, d) = (a+c, b+d)

return2 :: a -> a
return2 b = b

data Color = Red | Green | Blue
	deriving (Show, Read , Eq, Ord)

data OK = OK
	deriving( Read)

instance Show OK where 
	show _ = "OK"

instance Test OK where
	test _ = "fuck you"

instance (Test a ) where
	test _ = "fuck yourself"

--instance Read OK where
--	read _ = OK

data ThisWorks = ThisWorks OK 
	deriving (Show)

class Test a where
	test:: a -> String

data Maybe a  = Maybe a
	deriving (Show)

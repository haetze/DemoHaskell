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

first :: [a] -> a
first (x:xs) = x
first2:: (a, b) -> a
first2 (a, b) = a

mult :: Int -> Int -> Int
mult a b = (a)*(b)

times2 :: Double -> Double
times2 a = 2*a

(*+*) :: Double -> Double -> Double
b *+* a  = (a+b)*(b+a) 

(+++) :: (Double, Double) -> (Double, Double) -> (Double, Double)
(a, b) +++ (c, d) = (a+c, b+d)

return2 :: a -> a
return2 b = b

ownLast:: [a] -> a
ownLast [] = error "empty List"  
ownLast (x:[]) = x
ownLast (_:xs) = ownLast xs

ownConcat:: [[a]] 	-> [a]
ownConcat [] 		= []
ownConcat (x:xs) 	= x ++ ownConcat (xs) 

loop:: Int -> (Int -> Int -> Int) -> [Int] -> Int 
loop acc func (x:xs) = loop (func acc x) func xs
loop acc _ [] = acc

--tailss:: [a] -> [[a]]
tailss [] = []
tailss (_:xs) = xs : (tailss xs)


data Color = Red | Green | Blue
	deriving (Show, Read , Eq, Ord)

data BookInfo = Book Int String [String]
	| BookAn Int String 
	deriving (Show, Read, Eq)

getId :: BookInfo -> Int
getId (Book id _ _) = id
getId (BookAn id _) = id

data Book = BookRecord {
		id2 ::Int,
		name ::String,
		add ::[String]
	}
	| BookRecordAn{
		id2 ::Int,
		add ::[String]
	}
	deriving(Show, Read, Eq) 


data OK = OK
	deriving( Read)

instance Show OK where 
	show _ = "OK"

instance Test OK where
	test _ = "fuck you"

instance (Test a ) where
	test _ = "fuck yourself"

instance Num [Char] where
	a + b = a ++ b
	a - b = a ++ "-" ++ b
	a * b = ""
	abs a = a
	signum a = a
	fromInteger a = 0  	

--instance Read OK where
--	read _ = OK

data ThisWorks = ThisWorks OK 
	deriving (Show)

class Test a where
	test:: a -> String

data Maybe a  = Maybe a
	deriving (Show)


data ObcJ = ObcJ{
	nameVa ::String,
	val :: JSVal
}

data JSVal = 
	JSString String |
	JSInt Int |
	JSDouble Double |
	JSOb ObcJ |
	JSOb2 [ObcJ]


instance Show ObcJ where
	show (ObcJ nameVa val)  = "{ " ++ nameVa  ++ " : " ++ show ( val ) ++ "}" 

instance Show JSVal where 
	show (JSString s) = s
	show (JSInt i) = show i
	show (JSDouble d) = show d
	show (JSOb ob) = show ob
	show (JSOb2 []) = ""
	show (JSOb2 (x:[])) = show x 
	show (JSOb2 c) = show x ++ "," ++ show (JSOb2 xs)
		where x:xs = c

routOneLevelLast (JSOb2 b)  = xs
	where _:xs = b
routOneLevelFirst (JSOb2 b)  = x 
	where x:_ = b

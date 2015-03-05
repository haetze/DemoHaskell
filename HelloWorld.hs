--Hello World
--4.1.2014
import System.Environment(getArgs)

type Date = Int
type Name = [String]

data Birth = Birth{
	 name :: Name,
	date :: Date
} deriving(Show)



main = do
	a <- getArgs
	let hello  = "Hello Worl1d"
	let birth  = Birth a 22041997
	let (firstName: _) = name birth
	putStrLn $ show firstName


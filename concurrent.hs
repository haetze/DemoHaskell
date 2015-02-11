--module Con( --	)where 
import Control.Concurrent
import Control.Parallel
import System.Environment
import Control.Parallel.Strategies
import Logger
import Control.Monad.Par.Scheds.Trace
import System.IO

{-
main = do
	args <- getArgs
	let [a, b] = map read args
 	putStr . show $ 
		runPar $ do
		i <- new
		j <- new
		fork (put i (fib a))
		fork (put j (fib b))
		c <- get i
		d <- get j
		return (c, d)-}

main = do 
	args <- getArgs
	let x = map read args
	let a = map fib2 x `using` parList (rparWith rpar)
	writeFile "sol" . show $ a

fib:: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib2:: Integer -> Integer
fib2 a = fibber 0 1 (a-1)

fibber:: Integer -> Integer -> Integer -> Integer
fibber a b 0 = a + b
fibber a b n = fibber b (a+b) (n-1)



s f = do
	i <- forkIO $ f "sss\n"
	print i
	

--runEval:: Eval a -> a
m:: ([Int], [Int])
m = runEval $ do
	a <- rpar $ incEach [1..100000000000]
	b <- rpar $ incEach [1..100000000000]
	return (a,b)

incEach::Num a => [a] -> [a]
incEach [] = []
incEach (x:xs) = (x+1:incEach xs)

evalList2 ::Strategy a -> Strategy [a]
evalList2 str [] = return []
evalList2 str (x:xs) 	= do
	x' <- str x
	xs' <- evalList2 str xs
	return (xs'++[x'])

parList2 :: Strategy a -> Strategy [a]
parList2 str = evalList2 (rparWith str)


--Compute a list of 'same kind' problems in parallel using:
--solution = map solver [problems] `usingg` parList strategy

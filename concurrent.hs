

--module Con(
--	)where
import Control.Concurrent
import Control.Parallel
import System.Environment
import Control.Parallel.Strategies
import Logger
import Control.Monad.Par.Scheds.Trace


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
		return (c, d)

fib:: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)




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

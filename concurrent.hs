import Control.Concurrent
import Control.Parallel
import Control.Parallel.Strategies


main = do
	let x = m
	print "done"


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
	return (x':xs')

parList2 :: Strategy a -> Strategy [a]
parList2 str = evalList2 (rparWith str)


--Compute a list of 'same kind' problems in parallel using:
--solution = map solver [problems] `usingg` parList strategy




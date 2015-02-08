import Control.Concurrent
import Control.Parallel
import Control.Parallel.Strategies

s f = do
	i <- forkIO $ f "sss\n"
	print i
	

--runEval:: Eval a -> a
m:: ([Int], [Int])
m = runEval $ do
	a <- rpar $ incEach [1..100000000000]
	b <- rpar $ incEach [1..100000000000]
	return (a,b)

main = do
	let x = m
	print "done"

incEach::Num a => [a] -> [a]
incEach [] = []
incEach (x:xs) = (x+1:incEach xs)

incEachM::Num a => [a] -> Eval [a]
incEachM a = return (incEach a)




#! /usr/bin/env runhugs +l
--
-- FibMain.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--
import Fib
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.Environment(getArgs)


main = do
	args <- getArgs
	chan1 <- newChan
	forkIO . server chan1 $ map read args
	rec chan1


server:: Chan (Maybe Integer) -> [Int] -> IO ()
server chan (x:xs) = exec chan (x:xs) [2,1,1]

exec:: Chan (Maybe Integer) -> [Int] -> [Integer] -> IO ()
exec chan (x:[]) ys = do 
	writeChan chan y
	writeChan chan $ Just (-1)
	where
		(n, zs) = fib ys x
		y       = get zs n
exec chan (x:xs) ys = do
	writeChan chan y
	exec chan xs zs	
	where
		(n, zs) = fib ys x
		y       = get zs n


rec:: Chan (Maybe Integer) -> IO ()
rec chan = do 
	a <- readChan chan 
	case a of
	  Just (-1) -> putStr "done"
	  Just n    -> do 
		putStr $ show n ++ "\n"
		rec chan
	  Nothing -> do
		putStr "failed to get index from List"

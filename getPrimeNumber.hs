#! /usr/bin/env runhugs +l
--
-- getPrimeNumber.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

import PrimeNumber
import Fib(get)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.Environment(getArgs)


main = do
	args <- getArgs
	chan1 <- newChan
	forkIO . server chan1 $ map read args
	rec chan1


server:: Chan (Int, (Maybe Integer)) -> [Int] -> IO ()
server chan (x:xs) = exec chan (x:xs) [3,2]

exec:: Chan (Int, (Maybe Integer)) -> [Int] -> [Integer] -> IO ()
exec chan (x:[]) ys = do 
	writeChan chan (x, y)
	writeChan chan $ (0, Just (-1))
	where
		(n, zs) = getPrime ys x
		y       = get zs n
exec chan (x:xs) ys = do
	writeChan chan (x, y)
	exec chan xs zs	
	where
		(n, zs) = getPrime ys x
		y       = get zs n


rec:: Chan (Int, (Maybe Integer)) -> IO ()
rec chan = do 
	(b, a) <- readChan chan 
	case a of
	  Just (-1) -> putStr "done"
	  Just n    -> do 
		putStr $ show b ++ ":" ++ show n ++ "\n"
		rec chan
	  Nothing -> do
		putStr "failed to get index from List"

getPrime:: PrimeNumbers  -> Int -> (Int, PrimeNumbers)
getPrime (x:xs) a | a<=0 = (a, (x:xs))
		  | length (x:xs) > a = (n, (x:xs))
		  | length (x:xs) == a = (1, (x:xs))
		  | length (x:xs) < a = getPrime (nextPrime (x:xs)) a
	where 
		n = (length xs) - a + 2 

#! /usr/bin/env runhugs +l
--
-- Demo5.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Demo5 where


import Demo4
import Control.Concurrent
import Control.Concurrent.Chan

calcParCon:: (Chan b -> a -> IO ()) -> (Chan b -> a -> IO ()) -> (a, a) -> IO (b,b)
calcParCon f1 f2  (c, d)= do
	ch1 <- newChan
	ch2 <- newChan
	forkIO $ f1 ch1 c 
	forkIO $ f2 ch2 d
	a <- readChan ch1
	b <- readChan ch2
	return (a, b)

	
f:: Chan Integer -> Integer -> IO ()
f ch a = writeChan ch $ fib2 a


	

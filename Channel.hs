#! /usr/bin/env runhugs +l
--
-- Channel.hs
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
--
-- Distributed under terms of the MIT license.
--

module Channel where


import Control.Concurrent
import Control.Concurrent.Chan
import Con

forker:: (Chan Integer -> Integer -> IO ()) -> (Chan Integer -> Integer -> IO() ) -> IO (Integer, Integer)
forker f1 f2 = do
	ch1 <- newChan
	ch2 <- newChan
	forkIO $ f1 ch1 12
	forkIO $ f2 ch2 121
	a <- readChan ch1
	b <- readChan ch2
	return (a, b)
	

f1:: Chan Integer -> Integer ->  IO () 
f1 a s = writeChan a $ fib2 s

f2:: Chan Integer -> Integer -> IO ()
f2 a s = writeChan a $ fib2 s

#! /usr/bin/env runhugs +l
--
-- demo5.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--


import Demo4
import System.Environment(getArgs)

main = do
	args <-getArgs
	let a = doppMapPar read fib2 args
	print a




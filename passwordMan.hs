#! /usr/bin/env runhugs +l
--
-- passwordMan.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE BangPatterns #-}

import Data.Maybe
import System.Environment(getArgs)
import System.IO
import PasswordMan

main:: IO ()
main = do
	args <- getArgs
	case args of
		("lookupUserAt":s:u:_) ->do
			 file <- readFile "/home/haetze/passwords" 
			 let pwd = read file
			 putStr . pre $ lookupUserAtService u s pwd
		("lookupService":s:_) ->do 
			 file <- readFile "/home/haetze/passwords" 
			 let pwd = read file
			 shower $ lookupService s pwd
		("pipe":s:u:_) -> do
			file <- readFile "/home/haetze/passwords"
			let pwd = read file
			let ac = lookupUserAtService u s pwd
			case ac of
				Nothing -> putStrLn "fail"
				Just s -> putStr $ createPipeString s
		("update":s:u:p:_) -> do
			h <- openFile "/home/haetze/passwords" ReadMode
			f <- hGetContents h
			let !pwd = read f 
			let nP = update s u p pwd 
			hClose h
			writeToDisk $ show nP
		("insert":s:u:p:_) -> do 
			h <- openFile "/home/haetze/passwords" ReadMode
			f <- hGetContents h
			let !pwd = read f 
			let nP = insert s u p pwd 
			hClose h
			writeToDisk $ show nP
		("remove":s:u:_) -> do 
			h <- openFile "/home/haetze/passwords" ReadWriteMode
			f <- hGetContents h
			let !pwd = read f 
			let nP = remove s u pwd 
			hClose h
			writeToDisk $ show nP
		_ -> putStrLn $ "Command: lookupUserAt <User> <Service> \n lookupService <Service> \n " ++
			"update <Service> <User> <Password> \n insert <Service> <User> <Password> \n remove <Service> <User>"


shower:: Maybe [SUP] -> IO ()
shower Nothing 	     = putStrLn "nothing found"
shower (Just (x:[])) = putStr $ presentAccount x
shower (Just (x:xs)) = do
	putStr $ presentAccount x
	shower $ Just xs

pre:: Maybe SUP -> String
pre Nothing = "nothing found" 
pre (Just s) = presentAccount s

writeToDisk:: String -> IO ()
writeToDisk s = writeFile "/home/haetze/passwords" s

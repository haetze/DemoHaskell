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
			 pwd <- createPasswordsFromFileURL "/home/haetze/passwords"
			 putStr . pre $ lookupUserAtService u s pwd
		("lookupService":s:_) ->do 
			 pwd <- createPasswordsFromFileURL "/home/haetze/passwords"
			 shower $ lookupService s pwd
		("showAll":_) -> do
			 pwd <- createPasswordsFromFileURL "/home/haetze/passwords"
			 putStr $ presentAccountsInFile pwd
		("update":s:u:p:_) -> do
			!pwd <- createPasswordsFromFileURL "/home/haetze/passwords"
			let nP = update s u p pwd 
			writeToDisk $ show nP
		("insert":s:u:p:_) -> do 
			!pwd <- createPasswordsFromFileURL "/home/haetze/passwords"
			let nP = insert s u p pwd 
			writeToDisk $ show nP
		("remove":s:u:_) -> do 
			!pwd <- createPasswordsFromFileURL "/home/haetze/passwords"
			let nP = remove s u pwd 
			writeToDisk $ show nP
		_ -> putStrLn $ "Command: lookupUserAt <User> <Service> \n lookupService <Service> \n " ++
			"update <Service> <User> <Password> \n insert <Service> <User> <Password> \n remove <Service> <User> \n showAll"


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

#! /usr/bin/env runhugs +l
--
-- passwordMan.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--


import Data.Maybe
import System.Environment(getArgs)
import System.IO
import PasswordMan

main:: IO ()
main = do
	appendFile "~/.passwords" ""
	file <- readFile " ~/.passwords"
	let pwd = read file
	args <- getArgs
	case args of
		("lookupUserAt":u:s:_) -> putStr . pre $ lookupUserAtService u s pwd
		("lookupService":s:_) -> shower $ lookupService s pwd
		("update":s:u:p:_) -> do 
			let nP = update s u p pwd 
			writeFile "~/.passwords" $ show nP
		("insert":s:u:p:_) -> do
			let nP = insert s u p pwd 
			writeFile "~/.passwords" $ show nP
		("remove":s:u:_) -> do
			let nP = remove s u pwd
			writeFile "~/.passwords" $ show nP
		_ -> putStrLn "unkown Command"


shower:: Maybe [SUP] -> IO ()
shower Nothing 	     = putStrLn "nothing found"
shower (Just (x:[])) = putStr $ presentAccount x
shower (Just (x:xs)) = do
	putStr $ presentAccount x
	shower $ Just xs

pre:: Maybe SUP -> String
pre Nothing = "nothing found" 
pre (Just s) = presentAccount s

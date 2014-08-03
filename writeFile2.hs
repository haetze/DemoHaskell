-- writeFile2.hs
--
--
--
{-#LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

import System.IO
import System.Directory( removeFile)


main :: IO()
main = do
	f <- openFile "comment" AppendMode
	hClose f	 
	putStrLn "that short comment would you like to safe to the comment in this directory"
	input <- getLine
	fileR <- openFile "comment" ReadMode
	!fileString <-  (hGetContents fileR)
	--let t = fileString `deepseq` ()
	--putStrLn fileString
	let str = fileString ++ input ++ "\n" 
	--fileW <- openFile "comment2" WriteMode
	--hPutStr fileW str 
	hClose fileR
	--hClose fileW
	fileW2 <- openFile "comment" WriteMode
	hPutStr fileW2 str
	hClose fileW2
	--removeFile "comment2"

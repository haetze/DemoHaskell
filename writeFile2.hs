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
	putStrLn "that short comment would you like to safe to the comment in this directory"
	input <- getLine
	fileR <- openFile "comment" ReadWriteMode
	hSeek fileR SeekFromEnd 0
	t <- hIsEOF fileR
	if t
		then hPutStrLn fileR input
	else putStrLn "error"
	hClose fileR


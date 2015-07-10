#! /usr/bin/env runhugs +l
--
-- httpServer.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE BangPatterns #-}

import Network
import Control.Concurrent
import System.IO

main = withSocketsDo $ do
	sock <- listenOn $ PortNumber 1234
	loop sock
	where
		loop sock = do
			(h,_,_) <- accept sock
			body h
			loop sock

		body ha = do
			--s <- hGetContents ha
			--putStr s
			hPutStr ha $ createHTTPStringFromContent "test"
			hFlush ha
			hClose ha


createHTTPStringFromContent s = "HTTP/1.0 200 OK\r\nContent-Length: "++ (show $ length s )++ "\r\n\r\n" ++ s++ "\r\n"







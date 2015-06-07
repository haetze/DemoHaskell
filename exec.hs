#! /usr/bin/env runhugs +l
--
-- exec.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

import LambdaSubs
import LambdaTypes
import System.IO
import Data.Map

main = exec empty 0 

exec:: VarMap -> Int -> IO ()
exec m n = do
	l <- getLine
	case (words l) of
	 "Var":x -> exec (insert n (read (unwords x)) m) (n+1)
	 "Abs":x -> exec (insert n (read (unwords $ "Abs":x)) m) (n+1)
	 "App":_ -> putStrLn "Apps"
	 _ -> putStrLn "trash"



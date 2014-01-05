-- List Increment
-- 4.1.2014
--
--

import System.Environment (getArgs)

function [] [] = [] 
function (x:xs) n  = function xs ( n ++[ show(1+ read(x)) ])
function [] ns =  ns

	

main = do 
		args <- getArgs 
		mapM putStrLn (function args [])

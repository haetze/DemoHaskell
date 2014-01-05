-- List Increment
-- 4.1.2014
--
--

import System.Environment (getArgs)

function x = putStrLn (show ( (read x)+1))

main = do 
		args <- getArgs
		mapM function args	

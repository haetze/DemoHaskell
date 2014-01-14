-- writeFile.hs
--
import System.IO

main = do 
	input <- getLine
	appendFile "safeFile"  ("\n" ++ input)

{-- snippet main --}
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)
import Data.Char(toUpper)


interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = map toUpper

-- SafeRead.hs
-- Example Safe read


import Text.Read(readMaybe)

addMaybe::(Num a, Read a) =>  String -> String -> Maybe a
addMaybe s s' = do
  x <- readMaybe s
  y <- readMaybe s'
  return $ x + y

x :: Maybe Int
x = addMaybe "1232" "984"

x' :: Maybe Double
x' = addMaybe "1232" "984" 

x'' :: Maybe Int
x'' = addMaybe "1232.1" "984" 

module Lib
    ( someFunc
    , myMap
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myMap
  :: (a -> b)
  -> [a]
  -> [b]
myMap _ [] = []
myMap f (a:as) = f a : myMap f as

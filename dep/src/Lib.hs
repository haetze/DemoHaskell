{-# LANGUAGE TypeOperators #-}
module Lib
    ( someFunc
    , myMap
    , circ
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type a <~ b = b -> a

myMap
  :: (a -> b)
  -> [a]
  -> [b]
myMap _ [] = []
myMap f (a:as) = f a : myMap f as

circ
  :: (c <~ b)
  -> (b <~ a)
  -> (c <~ a)
circ = (.)  

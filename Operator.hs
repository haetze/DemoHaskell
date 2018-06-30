-- Operator.hs
-- Test of operators as input to function


module Operator where

-- Here a operator (#) is used as a variable to bind by the function call
fold (#) acc [] = acc
fold (#) acc (x:xs) = fold (#) (acc # x) xs

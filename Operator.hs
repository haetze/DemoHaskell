-- Operator.hs
-- Test of operators as input to function


module Operator where


fold (#) acc [] = acc
fold (#) acc (x:xs) = fold (#) (acc # x) xs

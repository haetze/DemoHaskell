#! /usr/bin/env runhugs +l
--
-- Compose.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Compose where


f::(Num a, Read a) => [Char] -> [a]
f = mapper read

mapper::( [a] -> b) -> [a] -> [b]
mapper _ [] = []
mapper f (a:as) = f [a] : mapper f as

plusOne::(Num a, Read a, Show a) => a -> a
plusOne = toString . map (+1) . f . show

toString::(Num a, Show a, Read a) => [a] -> a
toString = read . concat . map show 


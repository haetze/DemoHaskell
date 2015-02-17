#! /usr/bin/env runhugs +l
--
-- Demo6.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Demo6 where

import Data.Maybe


lookup2::Eq a =>[(a, b)] -> a -> Maybe (a,b)
lookup2 [] _ = Nothing
lookup2 ((x,y):xs) s | s == x = Just (x,y)
		     | s /= x = lookup2 xs s


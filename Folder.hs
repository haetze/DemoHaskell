#! /usr/bin/env runhugs +l
--
-- Folder.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Folder where


folder::[a] -> (a->a->a) -> a
folder (x:[]) _ = x
folder (x:xs) f = folder (a:as) f
	where 
	a = f x (head xs)
	as = tail xs


folderL::[a] -> (a->a->a) -> a
folderL xs f = folder (reverse xs) f

#! /usr/bin/env runhugs +l
--
-- demo3.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--
import System.IO

f:: Handle -> IO Handle
f a = return a

op:: String -> IO Handle
op s = openFile s ReadWriteMode


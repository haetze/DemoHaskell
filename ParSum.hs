#! /usr/bin/env runhugs +l
--
-- ParSum.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module ParSum where


import Control.Parallel
import System.Environment
import Control.Parallel.Strategies


sumP::Num a => [a] -> a
sumP (x:[]) = x
sumP (x:y:[]) = x + y
sumP (x:xs) = sumP ( map sumP (dive (x:xs)) `using` parList (rparWith rpar))


--naive implementation of dive;
--TODO: Balance it

dive::[a] -> [[a]]
dive [] = []
dive (x:[]) = [[x]]
dive (x:xs) = [(x:head xs:[]),(tail xs)]



#! /usr/bin/env runhugs +l
--
-- Random.hs
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
--
-- Distributed under terms of the MIT license.
--

module Random where


import System.Random

randomNumber ::Random a => (a, a) ->  IO a
randomNumber range = do
	g <- newStdGen
	let [x] = take 1 $ randomRs range g 
	return x

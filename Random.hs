#! /usr/bin/env runhugs +l
--
-- Random.hs
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
--
-- Distributed under terms of the MIT license.
--

module Random where


import System.Random

randomNumber ::Random a => (a, a) ->   IO a
randomNumber range = do
	g <- newStdGen
	let [x] = take 1 $ randomRs range g 
	return x



randomNumbers ::(Random a, Num a) => [a] -> IO [a]
randomNumbers [] = return []
randomNumbers (_:xs) = do 
	x <- randomNumber (0, 1000) 
	xs <- randomNumbers xs
	return (x:xs)

nRandomNumbers::Int -> IO [Int]
nRandomNumbers n = randomNumbers [1..n]


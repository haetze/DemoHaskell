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
	x <- randomNumber (0, 10) 
	xs <- randomNumbers xs
	return (x:xs)

nRandomNumbers::Int -> IO [Int]
nRandomNumbers n = randomNumbers [1..n]


findNumber::(Num a, Eq a) => a -> [a] -> [a]
findNumber _ [] = []
findNumber n (x:xs) | n == x = []
		    | n /= x = x : (findNumber n xs)

findNumberInMonad:: (Num a, Eq a, Monad m) => a -> [a] -> m [a]
findNumberInMonad n xs = return (findNumber n xs)

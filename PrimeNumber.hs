#! /usr/bin/env runhugs +l
--
-- PrimeNumber.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module PrimeNumber where


import Control.Parallel.Strategies(parMap, rpar)

primesTil:: Integer -> [Integer]
primesTil n = takeWhile (<n) $ sieve [2..]

isPrime n = and $ parMap rpar (\x -> n `mod` x /= 0) [2..n-1]

sieve (p:ps) = p : sieve [p' | p' <- ps, p' `mod` p /= 0]

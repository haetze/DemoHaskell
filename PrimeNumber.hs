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
primesTil 0 = []
primesTil 1 = []
primesTil n = if isPrime then n:primes else primes
  where
    primes = primesTil $ n - 1
    isPrime = and $ parMap rpar (\x -> n `mod` x /= 0) primes 

isPrime n = and $ parMap rpar (\x -> n `mod` x /= 0) [2..n-1]

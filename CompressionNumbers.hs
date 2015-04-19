#! /usr/bin/env runhugs +l
--
-- CompressionNumbers.hs
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
--
-- Distributed under terms of the MIT license.
--

module CompressionNumbers where

compressNumber::(Fractional a, Num a) => a -> a
compressNumber x = x/20

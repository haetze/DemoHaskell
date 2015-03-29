#! /usr/bin/env runhugs +l
--
-- Test.hs
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
--
-- Distributed under terms of the MIT license.
--

module Test where


data Rec a = Rec {
	val :: a,
	valToString :: a -> String
}


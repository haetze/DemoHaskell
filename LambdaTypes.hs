#! /usr/bin/env runhugs +l
--
-- LambdaTypes.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module LambdaTypes where

import Data.Map

data Term = Var Int
	| Abs Term
	| App Term Term
	deriving(Read, Show, Eq)


type VarMap = Map Int Term
 

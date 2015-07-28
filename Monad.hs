#! /usr/bin/env runhugs +l
--
-- Monad.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module Monad where

import Control.Applicative


data M a = M a | Fail
	deriving(Show, Read, Eq)


instance Monad (M) where
	return x = M x
	(M x) >>= f = f x
	Fail >>= _ = Fail

instance Functor M where
	fmap f (M a) = M $ f a

instance Applicative M where
	pure x = M x
	(<*>) (M x) (M y) = M (x y)

g s = fmap show s

f x = return x

#! /usr/bin/env runhugs +l
--
-- Test.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module Test where

inMonad::(Monad m, Show a) => a -> m String 
inMonad a = return $ show a 

data Rec a = Rec {
	val :: a,
	valToString :: a -> String
}

dd :: IO String
dd = inMonad "hi"

data L a = L String a  
	deriving(Show, Read)

instance Monad L where
	return x    = L "" x
	L _ x >>= f = f x
	_     >> f  = f
	--fail s	    = L "" s
	--


test::(Num a, Monad m) => m a -> m a
test m = m >>= (\a -> return (a+1))
 


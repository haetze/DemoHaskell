#! /usr/bin/env runhugs +l
--
-- Test.hs
<<<<<<< HEAD
-- Copyright (C) 2015 haetze <haetze@ubuntu>
=======
-- Copyright (C) 2015 haetze <haetze@haetze-MacBookAir>
>>>>>>> origin/master
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

>>>>>>> origin/master

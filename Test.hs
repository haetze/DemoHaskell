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

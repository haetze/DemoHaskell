#! /usr/bin/env runhugs +l
--
-- PasswordMan.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module PasswordMan where

import Data.Maybe


type Username = String
type PWD = String
type Service = String

data SUP = SUP Service Username PWD
	deriving(Show, Read)

data Passwords = Passwords [SUP]
	deriving(Show, Read)

createPasswordList:: Passwords
createPasswordList = Passwords []

presentAccount:: SUP -> String
presentAccount (SUP s u p) = "Account at " ++ s ++ ":" ++ u ++ "->" ++ p ++ "\n"

createPipeString:: SUP -> String
createPipeString (SUP s u p) = u ++ "\n" ++ p ++ ""

update:: Service -> Username -> PWD -> Passwords -> Passwords
update s u p pwd = case checkForExistense s u pwd of
	True -> insert s u p a
	False -> insert s u p pwd 
	where
		a = remove s u pwd 


insert:: Service -> Username -> PWD -> Passwords -> Passwords
insert s u p pwd = case checkForExistense s u pwd of
	False -> (SUP s u p) <> pwd 
	True -> update s u p pwd 

remove:: Service -> Username -> Passwords -> Passwords
remove s u p = case checkForExistense s u p of
	False -> p 
	True -> remover [] s u p

remover:: [SUP] -> Service -> Username -> Passwords -> Passwords
remover a s u (Passwords ((SUP se us p):xs)) | s == se && us == u = Passwords (a ++ xs)
					    | otherwise = remover ((SUP se us p):a) s u (Passwords xs)

checkForExistense:: Service -> Username -> Passwords -> Bool
checkForExistense _ _ (Passwords []) = False
checkForExistense service user (Passwords ((SUP s u _):xs)) | service == s && user == u = True
							   | otherwise = checkForExistense service user $ Passwords xs

checkForExistenseService:: Service -> Passwords -> Bool
checkForExistenseService _ (Passwords []) = False
checkForExistenseService service (Passwords ((SUP s _ _):xs)) | service == s = True
							   | otherwise = checkForExistenseService service $ Passwords xs

lookupService:: Service -> Passwords -> Maybe [SUP]
lookupService s pwd = case checkForExistenseService s pwd of
	False -> Nothing
	True -> Just $ upLooker s pwd

upLooker:: Service -> Passwords -> [SUP]
upLooker s (Passwords []) = []
upLooker s (Passwords ((SUP se u p):xs)) | s == se = (SUP se u p) : upLooker s (Passwords xs)
					      | otherwise = upLooker s (Passwords xs)

lookupUserAtService:: Username -> Service -> Passwords -> Maybe SUP
lookupUserAtService u s pwd = case lookupService s pwd of 
	Nothing -> Nothing
	Just xs -> findUser u xs

findUser:: Username -> [SUP] -> Maybe SUP
findUser u [] = Nothing
findUser u ((SUP s us p):xs) | u == us = Just (SUP s us p)
			     | otherwise = findUser u xs

(<>) :: SUP -> Passwords -> Passwords
a <> (Passwords as) = Passwords (a:as)


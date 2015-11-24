#! /usr/bin/env runhugs +l
--
-- passwordMan.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

{-# LANGUAGE BangPatterns #-}

import Data.Maybe
import System.Environment(getArgs)
import System.IO
import System.Directory
import PasswordMan

main:: IO ()
main = do
  home <- getHomeDirectory
  checkpassWordFile home
  args <- getArgs
  case args of
    ("lookupUserAt":s:u:_) ->do
       pwd <- createPasswordsFromFileURL $ home++"/.passwords"
       putStr . pre $ lookupUserAtService u s pwd
    ("lookupService":s:_) ->do 
       pwd <- createPasswordsFromFileURL $ home++"/.passwords"
       shower $ lookupService s pwd
    ("showAll":_) -> do
       pwd <- createPasswordsFromFileURL $ home++"/.passwords"
       putStr $ presentAccountsInFile pwd
    ("update":s:u:p:_) -> do
      !pwd <- createPasswordsFromFileURL $ home++"/.passwords"
      let nP = update s u p pwd 
      writeToDisk $ show nP
    ("insert":s:u:p:_) -> do 
      !pwd <- createPasswordsFromFileURL $ home++"/.passwords"
      let nP = insert s u p pwd 
      writeToDisk $ show nP
    ("remove":s:u:_) -> do 
      !pwd <- createPasswordsFromFileURL $ home++"/.passwords"
      let nP = remove s u pwd 
      writeToDisk $ show nP
    ("createPassword":_) -> do
      p <- createStandartPassword
      putStr $ "The password created for you is: " ++ p ++ "\n"
    ("createUser":s:u:_) -> do
      !p <- createPasswordsFromFileURL $ home++"/.passwords"
      nP <- createAccountForService s u p
      writeToDisk $ show nP 
      putStrLn . pre $ lookupUserAtService u s nP
    _ -> putStrLn $ "Command: lookupUserAt <User> <Service> \n lookupService <Service> \n " ++
      "update <Service> <User> <Password> \n insert <Service> <User> <Password> \n remove <Service> <User> \n showAll\n createPassword\n createUser <Service> <User>\n"


shower:: Maybe [SUP] -> IO ()
shower Nothing        = putStrLn "nothing found"
shower (Just (x:[])) = putStr $ presentAccount x
shower (Just (x:xs)) = do
  putStr $ presentAccount x
  shower $ Just xs

pre:: Maybe SUP -> String
pre Nothing = "nothing found" 
pre (Just s) = presentAccount s

writeToDisk:: String -> IO ()
writeToDisk s = do 
  home <- getHomeDirectory
  writeFile (home ++ "/.passwords") s

checkpassWordFile home = do
  con <- getDirectoryContents home
  case (elem ".passwords" con) of
   True -> return ()
   False -> do 
    putStrLn "passwords file missing, a new (empty) is created"
    createPasswordsFile home "/.passwords"




{-# LANGUAGE FlexibleInstances #-}

module Bingo where

import System.IO
import System.Directory
import Data.List

checkCurrentDirectoryWithPredicate::BooleanFileSearch a => (FilePath -> IO a) ->  IO [a]
checkCurrentDirectoryWithPredicate p = do
  path <- getCurrentDirectory
  checkDirWithPredicate path p

checkFileWithPredicate::BooleanFileSearch a =>  FilePath -> (FilePath -> IO a) -> IO a
checkFileWithPredicate fp p = do
  case ".txt" `isSuffixOf` fp of
    True -> p fp 
    False -> fmap falsefy (p fp) 

checkDirWithPredicate::BooleanFileSearch a =>  FilePath -> (FilePath -> IO a) -> IO [a]
checkDirWithPredicate dir p = do
  contents <- getDirectoryContents dir
  fs <- files contents
  ds <- dirs contents
  r <- mapM ((flip checkFileWithPredicate) p) fs
  dir_r <- mapM ((flip checkDirWithPredicate) p) (map (addPathToCurrentDir dir) ds)
  return $ (map (\b -> addToPath b dir) $ filter bool r ) ++  (concat dir_r)

files::[FilePath] -> IO [FilePath]
files fps = filter' doesFileExist  fps

dirs::[FilePath] -> IO [FilePath]
dirs fps = do
  f <- filter' (not <.> doesFileExist) fps
  let f2 = filter (\f -> f /= "..") f
  return $ filter (\f -> f /= ".") f2


addPathToCurrentDir:: FilePath -> FilePath -> FilePath
addPathToCurrentDir dir fp = dir ++ "/" ++ fp


(<.>):: Monad m => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g = \a -> do
  b <- g a
  return $ f b

filter':: Monad m => (a -> m Bool) -> [a] -> m [a]
filter' _ [] = return []
filter' p (x:xs) = do
  px <- p x
  pxs <- filter' p xs
  if px then return (x:pxs) else return pxs

--Konkrete Implementierung von Unterschiedlichen Typen

class BooleanFileSearch b where
  bool:: b -> Bool
  falsefy :: b -> b
  fp :: b -> FilePath
  addToPath:: b -> FilePath -> b
  
instance BooleanFileSearch (FilePath, Bool) where
  bool      = snd
  falsefy b = (fst b, False) 
  fp = fst
  addToPath b dir = (addPathToCurrentDir (fp b) dir , bool b)

type Line = Int

instance BooleanFileSearch (FilePath, Line, Bool) where
  bool (fp, l, b) = b
  falsefy (fp, l, b) = (fp, l, False)
  fp (f, l, b) = f
  addToPath (fp, l, b) dir = (addPathToCurrentDir fp dir, l, b)



checkCurrentDirectory:: IO [(FilePath, Bool)]
checkCurrentDirectory = checkCurrentDirectoryWithPredicate (\fp -> do
                                                               b <- fmap (elem "Bingo" . words) (readFile fp)
                                                               return (fp, b))



checkCurrentDirectoryWithLine:: IO [(FilePath, Line, Bool)]
checkCurrentDirectoryWithLine = checkCurrentDirectoryWithPredicate withLine
  where 
    withLine fp = do
      txt <- readFile fp
      let line  = lines txt
      let (l, b) = loop 0 line
      return (fp, l, b)
        where
          loop _ [] = (-1, False)
          loop n (x:xs) = if elem "Bingo" (words x) then (n, True) else loop (n+1) xs

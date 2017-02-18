module Bingo where

import System.IO
import System.Directory
import List



checkFile::FilePath -> IO (FilePath, Bool)
checkFile fp = do
  case isSuffix ".txt" fp of
    True -> do
      txt <- fmap words (readFile fp)
      return (fp, ("Bingo" `elem` txt))
    False -> return (fp, False)

checkDir:: FilePath -> IO [(FilePath, Bool)]
checkDir dir = do
  contents <- getDirectoryContents dir
  fs <- files contents
  ds <- dirs contents
  r <- mapM checkFile fs
  dir_r <- mapM checkDir (map (addPathToCurrentDir dir) ds)
  let dir_r_tmp = concat dir_r
  let f_r = map (\(fp, b) -> (addPathToCurrentDir dir fp, b)) $ filter snd r 
  return $ f_r ++  dir_r_tmp

files::[FilePath] -> IO [FilePath]
files fps = filter' doesFileExist  fps


dirs::[FilePath] -> IO [FilePath]
dirs fps = do
  f <- filter' (not <.> doesFileExist) fps
  let f2 = filter (\f -> f /= "..") f
  return $ filter (\f -> f /= ".") f2


addPathToCurrentDir:: FilePath -> FilePath -> FilePath
addPathToCurrentDir dir fp = dir ++ "/" ++ fp

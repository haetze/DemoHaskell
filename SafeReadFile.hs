-- Created on 05 Dec 2020 by richard.stewing@udo.edu

module SafeReadFile where


import System.IO
import Control.Exception


safeRead :: String -> IO (Maybe String)
safeRead path = (fmap Just $ readFile path) `catch` handleExists
  where
    handleExists :: IOException -> IO (Maybe String)
    handleExists _ = return Nothing


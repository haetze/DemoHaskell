{-# LANGUAGE EmptyCase #-}
-- LambdaCalc.hs
-- Simple LambdaCalc

module LambdaCalc where

import Control.Monad

{-
t := true
     false
     if t then t else t
     0
     succ t
     pred t
     isZero
-}


data T = TermTrue
       | TermFalse
       | ITE T T T
       | Zero
       | Succ T
       | Pred T
       | IsZero T deriving(Show)

data TAlg t = TAlg { true :: t
              , false :: t
              , ite :: t -> t -> t -> t
              , zero :: t
              , succ :: t -> t
              , pred :: t -> t
              , isZero :: t -> t
              }


readTrue:: String -> Maybe (T, String)
readTrue string = case words string of
  ("true":rest) -> Just (TermTrue, unwords rest)
  _             -> Nothing

readFalse:: String -> Maybe (T, String)
readFalse string = case words string of
  ("false":rest) -> Just (TermFalse, unwords rest)
  _             -> Nothing

readBool:: String -> Maybe (T, String)
readBool string = msum $ map ($string) [readFalse, readTrue]


readIf:: String -> Maybe String
readIf string = case words string of
  ("if":rest) -> Just $ unwords rest
  _             -> Nothing

readThen:: String -> Maybe String
readThen string = case words string of
    ("then":rest) -> Just $ unwords rest
    _             -> Nothing


readElse:: String -> Maybe String
readElse string = case words string of
  ("else":rest) -> Just (unwords rest)
  _             -> Nothing

readITE:: String -> Maybe (T, String)
readITE string = do
  s <- readIf string
  (t, s) <- readTerm s
  s <- readThen s
  (t', s) <- readTerm s
  s <- readElse s
  (t'', s) <- readTerm s
  return $ (ITE t t' t'', s)

readTerm:: String -> Maybe (T, String)
readTerm s = msum $ map ($s) [readFalse, readTrue, readITE]


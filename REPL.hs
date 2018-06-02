module REPL where

import Text.Read(readMaybe)
import Data.Char

data Operator = Plus
              | Mult
              | Sub
              | Div deriving(Eq, Read)

data OptTree = Op Operator OptTree OptTree
             | Number Int deriving(Eq, Read)


readChar:: Char -> String -> Maybe (Char, String)
readChar c (c':cs) | c == c' = Just (c, cs)
                  | True    = Nothing

readOperator:: String -> Maybe (Operator, String)
readOperator (c:cs) | c == '+' = Just (Plus, cs) 
                    | c == '*' = Just (Mult, cs) 
                    | c == '-' = Just (Sub,  cs) 
                    | c == '/' = Just (Div,  cs)
                    | True     = Nothing

readNumber:: String -> Maybe (OptTree, String)
readNumber s = do
  n <- readMaybe n
  return (Number n, s')
  where
    n  = takeWhile isDigit s
    s' = dropWhile isDigit s
  
readWord:: String -> String -> Maybe (String, String)
readWord word string = if word == w then Just (w, s) else Nothing
  where
    w = take (length word) string
    s = drop (length word) string

readExp:: String -> Maybe (OptTree, String)
readExp s = do
  (n, s)  <- readNumber s
  if s == ""
    then do return (n, s)
    else do (op, s) <- readOperator s
            if op == Mult || op == Div
              then do (m, s) <- readNumber s
                      if s == ""
                        then return (Op op n m, s)
                        else do (op', s) <- readOperator s
                                (exp, s) <- readExp s
                                return (Op op' (Op op n m) exp, s)
              else do (m, s)  <- readExp s
                      return (Op op n m, s)

readAndEval:: String -> Maybe (Int, String)
readAndEval s = do
  (t, s) <- readExp s
  return (eval t, s)

eval:: OptTree -> Int
eval (Number n) = n
eval (Op Plus n m) = eval n +     eval m
eval (Op Mult n m) = eval n *     eval m
eval (Op Div n m)  = eval n `div` eval m
eval (Op Sub n m)  = eval n -     eval m

  
instance Show OptTree where
  show (Number n)    = show n
  show (Op Plus n m) = show n ++ "+" ++ show m
  show (Op Mult n m) = show n ++ "*" ++ show m
  show (Op Sub n m)  = show n ++ "-" ++ show m
  show (Op Div n m)  = show n ++ "/" ++ show m

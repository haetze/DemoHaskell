{-# LANGUAGE EmptyCase #-}
-- LambdaCalc.hs
-- Simple LambdaCalc



module LambdaCalc where

import Control.Monad


-- Begin untyped arithmetric term
-- Losely based on "Types and Programming Languages" by Benjamin C. Pierce

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

data TAlg bool num = TAlg { true :: bool
                          , false :: bool
                          , ite :: bool -> Result bool num -> Result bool num -> Result bool num
                          , zero :: num
                          , succ' :: num -> num
                          , pred' :: num -> num
                          , isZero :: num -> bool
                          }

data Result bool num = Num num
                     | Bool bool deriving(Show)


simpleAlg:: TAlg Bool Int 
simpleAlg = TAlg { true = True
                 , false = False
                 , ite = \b n n' -> if b then n else n'
                 , zero = 0
                 , succ' = \n -> n+1
                 , pred' = \n -> n-1
                 , isZero = (==0)
                 }
lispAlg:: TAlg String String
lispAlg = TAlg { true   = "t"
                 , false  = "nil"
                 , ite    = \b n n' -> Num $ "(if (" ++ b ++ ") (" ++ f n ++ ") (" ++ f n ++"))" 
                 , zero   = "0"
                 , succ'  = \n -> show $ (read n :: Int)+1
                 , pred'  = \n -> show $ (read n :: Int)-1
                 , isZero = \n ->"(" ++ n ++ "==0)"
                 } where f (Bool b) = b
                         f (Num  n) = n


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


readZero:: String -> Maybe (T, String)
readZero ('0':rest) = Just $ (Zero, rest)
readZero _          = Nothing

readSucc:: String -> Maybe (T, String)
readSucc string = case words string of
  ("succ":rest) -> do
    (n, rest) <- readNum $ unwords rest
    return $ (Succ n, rest)
  _           -> Nothing

readPred:: String -> Maybe (T, String)
readPred string = case words string of
  ("pred":rest) -> do
    (n, rest) <- readNum $ unwords rest
    return $ (Pred n, rest)
  _           -> Nothing


readNum:: String -> Maybe (T, String)
readNum s = msum $ map ($s) [readZero, readSucc, readPred]

readIsZero:: String -> Maybe (T, String)
readIsZero string = case words string of
  ("isZero":rest) -> do
    (n, rest) <- readNum $ unwords rest
    return $ (IsZero n, rest)
  _           -> Nothing

readTerm:: String -> Maybe (T, String)
readTerm s = msum $ map ($s) [readFalse, readTrue, readITE, readNum]

fold:: TAlg bool num -> T -> Result bool num
fold alg t = case t of
  TermTrue -> Bool $ true alg
  TermFalse -> Bool $ false alg
  ITE t t' t'' -> ite alg (foldB alg t) (fold alg t') (fold alg t'')
  Zero         -> Num $ zero alg
  Succ t       -> Num $ succ' alg  $ foldN alg t
  Pred t       -> Num $ pred' alg  $ foldN alg t
  IsZero t     -> Bool $ isZero alg $ foldN alg t

foldB:: TAlg bool num -> T -> bool
foldB alg t = e where Bool e = fold alg t

foldN:: TAlg bool num -> T -> num
foldN alg t = e where Num e = fold alg t


readAndEvalAlg:: TAlg bool num -> String -> Maybe (Result bool num)
readAndEvalAlg alg s = do
  (t,_) <- readTerm s
  return $ fold alg t

readAndEvalSimple = readAndEvalAlg simpleAlg

  
repl:: (Show bool, Show num) => TAlg bool num ->  IO ()
repl alg = do
  putStr "<<< "
  s <- getLine
  case readAndEvalAlg alg s of
    Nothing -> do putStrLn "Malformed term"; repl alg 
    Just (Bool b) -> do putStrLn $ ">>> " ++ show b; repl alg  
    Just (Num n) -> do putStrLn $ ">>> " ++ show n; repl alg
    





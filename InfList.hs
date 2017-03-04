module InfList where


import Control.Concurrent.STM
import System.IO


-- Der rekursive Typ der hier verwendet werden wuerde fuehrt dazu, dass
-- Es nicht moeglich ist so ein Modul typisieren kann, also kann es auch nicht
-- compelieren kann 
-- type T = () -> (Integer, T)    

-- gen_fib:: Integer -> Integer -> (Integer, c)
--gen_fib a b = (b, \() -> (gen_fib b (a+b)))


fib = \a b -> (a, (b, a+b))

fromRoot = newTVarIO (fib, (0, 1))


step var = do
  (f, (a, b)) <- readTVar var
  let (n, (na, nb)) = f a b
  writeTVar var $ (f, (na, nb))
  return (f, n)

takeNSteps 0 var = do
  (_, n) <- step var
  return n
takeNSteps n var = do
  (_,_) <- step var
  takeNSteps (n-1) var

buildList 0 var = do
  return []
buildList n var = do
  (_,f) <- step var
  fs <- buildList (n-1) var
  return $ f : fs
  

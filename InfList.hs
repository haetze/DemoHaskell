module InfList where

-- Der rekursive Typ der hier verwendet werden wuerde fuehrt dazu, dass
-- Es nicht moeglich ist so ein Modul typisieren kann, also kann es auch nicht
-- compelieren kann 
-- type T = () -> (Integer, T)    

-- gen_fib:: Integer -> Integer -> (Integer, c)
gen_fib a b = (b, \() -> (gen_fib b (a+b)))

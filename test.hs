-- 31.1.2014
-- Richard Stewing 
-- test.hs
--
{-#LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--data Maybe a = Maybe a

(==>) :: Bool -> Bool -> Bool
a ==> f 
	| a == True  = f
	| a == False = True
 

test :: [Double ] -> Bool
test a = not (null a) ==> False

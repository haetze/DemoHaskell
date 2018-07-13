module Phil where

import Data.List(nub, (\\), sort)
 
type PhilState = ([Int], [Int], [Int])
type PhilTrans = (PhilState, PhilState)

--Links sitzen die "kleineren" Philosophen
--Rechts die groesseren
leftPhil, rightPhil:: Int -> Int -> Int
leftPhil n numberOfPhils = (n-1) `mod` numberOfPhils
rightPhil n numberOfPhils = (n+1) `mod` numberOfPhils

notLeft, notRght:: PhilState -> Int -> Bool
notLeft (_,st,_) x = not $ x `elem` st
notRght (_,_,st) x = not $ x `elem` st

philStart:: Int -> PhilState
philStart n = ([0..n-1], [], [])

philSucc:: PhilState -> [PhilTrans]
philSucc state@(idl, left, right) = fromIdle ++ fromLeft ++ fromRght
  where
    fromIdle = [(state, (s$ idl \\ [x], s$ x:left, s$ right))
               | x <- idl
               , notRght state $ leftPhil  x max]

    fromLeft = [(state, (s$ idl, s$ left, s$ x:right))
               | x <- left
               , notLeft state $ rightPhil x max]

    fromRght = [(state, (s$ x:idl, s$ left \\ [x], s$ right \\ [x]))
               |x <- right] 

    s        = sort . nub
    max = 1 + (maximum $ idl ++ left ++ right)
                  

philTransitions:: Int -> [PhilTrans]
philTransitions n = filterSelf $ concatMap (\s -> philSucc s) allStates
  where
    filterSelf trans = nub $ filter (\(x,y) -> x /= y) trans
    allStates = fixpt subset philStep [philStart n]
    subset st st' = all (`elem` st') st
    fixpt cmp phi start =  if nextStep `cmp` start 
                           then nextStep
                           else fixpt cmp phi nextStep
      where
        nextStep = phi start
    philStep states = nextStates
      where
        nextStates = nub $ (map snd nextTrans) ++ states
        nextTrans  = concatMap (\s -> philSucc s) states
  
printTransitions:: [PhilTrans] -> String
printTransitions trans = l ++ l' ++ "\n"
  where
    strings = map printTrans trans
    l = unlines $ map (\x -> x ++ " &") (init strings)
    l' = last strings
    printTrans:: PhilTrans -> String
    printTrans (st, st') = show st ++ " -> " ++ show st'


header:: Int -> String
header n = "axioms:\n" ++
           "states == [([0.." ++ show (n-1) ++ "], [], [])] & \n"

toFile:: Int -> IO ()
toFile n = writeFile ("phil" ++ show n) s
  where
    s = header n ++ transitionString
    transitionString = printTransitions transitions
    transitions = philTransitions n

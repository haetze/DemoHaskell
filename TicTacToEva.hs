#! /usr/bin/env runhugs +l
--
-- TicTacToEva.hs
-- Copyright (C) 2015 haetze <haetze@ubuntu>
--
-- Distributed under terms of the MIT license.
--

module TicTacToEva where


data Field = X
	| O
	| NotSet
	deriving(Show, Read, Eq)

type Line = (Field, Field, Field)

type Board = (Line, Line, Line)

element::Int -> (a,a,a) -> a
element 1 (x,_,_) = x
element 2 (_,x,_) = x
element 3 (_,_,x) = x
element _ _	  = error "number to large"

elementP::(a,b) -> a
elementP (x,_) = x

evaluateLine:: Line -> (Bool, Field)
evaluateLine (X,X,X) = (True, X)
evaluateLine (O,O,O) = (True, O)	
evaluateLine _	     = (False, NotSet)

evalDown:: Board -> (Bool, Field)
evalDown (a, b, c) = evaluateBoard ( (element 3 a, element 3 b, element 3 c) , (element 2 a, element 2 b, element 2 c),  (element 1 a, element 1 b, element 1 c))

evaluateCross:: Board -> (Bool, Field)
evaluateCross (a, b, c) |  evaluateLine (element 1 a, element 2 b, element 3 c) == (True, X) = evaluateLine (element 1 a, element 2 b, element 3 c) 
	| evaluateLine (element 3 a, element 2 b, element 1 c) == (True, X) = evaluateLine (element 3 a, element 2 b, element 1 c) 
evaluateCross (a, b, c) |  evaluateLine (element 1 a, element 2 b, element 3 c) == (True, O) = evaluateLine (element 1 a, element 2 b, element 3 c) 
	| evaluateLine (element 3 a, element 2 b, element 1 c) == (True, O) = evaluateLine (element 3 a, element 2 b, element 1 c) 
evaluateCross _ = (False, NotSet)


evaluateBoard:: Board -> (Bool, Field)
evaluateBoard (a, b, c) |  (elementP $ evaluateLine a )== True = evaluateLine a  
	|( elementP $ evaluateLine b )== True = evaluateLine b 
	|( elementP $ evaluateLine c) == True = evaluateLine c
	|(elementP $ evaluateCross (a, b, c) )== True = evaluateCross (a, b, c)
evaluateBoard a = evalDown a

checkBoard:: Board -> Bool
checkBoard a = dif a

change:: Field -> Int
change X = 1
change O = -1
change NotSet = 0

addLine:: Line -> Int
addLine (a, b, c) = (change a)+(change b)+(change c)

dif:: Board -> Bool
dif (a, b, c ) | (addLine a + addLine b + addLine c) < -1 ||  (addLine a + addLine b + addLine c) > 1 
		= False
dif _ 		= True 



a :: Board
a = ((X,O,O), (X,NotSet,NotSet),(X,NotSet,NotSet))

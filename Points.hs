#! /usr/bin/env runhugs +l
--
-- Points.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--

module Points where


type Point = (Double, Double)
data Set = Set [Point]
type Circle = (Point, Double)

distance:: Point -> Point -> Double
distance (x1, x2) (y1, y2) = sqrt (x + y)
	where 
		x = (x1-y1)^2
		y = (x2-y2)^2


pointPointCircle:: Point -> Point -> Circle
pointPointCircle p q = (p, distance p q)

pointInCircle:: Circle -> Point -> Bool
pointInCircle (q, n) p = n >= distance p q

setInCircle:: Set -> Circle -> Set
setInCircle (Set xs) c = Set $ filter (pointInCircle c) xs

cN:: Set -> Point -> Point 
cN (Set (x:[])) p = x
cN (Set (x:xs)) p = case length y of
		0 -> x 
		n -> cN (Set y) p
	where 
		c = pointPointCircle p x
		y = filter (pointInCircle c) xs


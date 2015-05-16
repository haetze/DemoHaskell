#! /usr/bin/env runhugs +l
--
-- Points.hs
-- Copyright (C) 2015 haetze <haetze@home>
--
-- Distributed under terms of the MIT license.
--lass Eq a => Ord a where
--  compare :: a -> a -> Ordering
--    (<) :: a -> a -> Bool
--      (>=) :: a -> a -> Bool
--        (>) :: a -> a -> Bool
--          (<=) :: a -> a -> Bool
--            max :: a -> a -> a
--              min :: a -> a -> a
--


{-#LANGUAGE TypeSynonymInstances, OverlappingInstances , FlexibleInstances#-}
module Points where
import Data.List


type Point = (Double, Double)
data Set = Set [Point]
type Circle = (Point, Double)

distance:: Point -> Point -> Double
distance (x1, x2) (y1, y2) = sqrt (x + y)
	where 
		x = (x1-y1)^2
		y = (x2-y2)^2

distanceAndPoint p q = (q, distance p q)


instance Eq  Circle where
	(p, d) == (q, e) = d == e
	x /= y 		 = not (x == y)

instance Ord Circle where
	(p, d) < (q, e) = d < e
	(p, d) >= (q, e) = d >= e
	(p, d) > (q, e) = d > e
	(p, d) <= (q, e) = d <= e
	b `min` a = case b == a of
 		True -> b
		False -> case b < a of
			True -> b
			False -> a
	a `max` b = case min b a of
		a -> b
		b -> a
	(p, d) `compare` (q, e) = compare d e
	
	
	


pointPointCircle:: Point -> Point -> Circle
pointPointCircle p q = (p, distance p q)

pointInCircle:: Circle -> Point -> Bool
pointInCircle (q, n) p = n >= distance p q

setInCircle:: Set -> Circle -> Set
setInCircle (Set xs) c = Set $ filter (pointInCircle c) xs

cN:: Set -> Point -> Circle 
cN (Set (x:[])) p = (x, distance x p)
cN (Set (x:xs)) p = head . sort . map (distanceAndPoint p) $ (x:xs)


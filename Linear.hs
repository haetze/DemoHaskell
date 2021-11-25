{-# LANGUAGE LinearTypes #-}
-- Created on 25 Nov 2021 by richard.stewing@udo.edu
-- Copyright Richard Stewing,25 Nov 2021
-- Licensed under GPLv3, See toplevel LICENSE file.

module Linear where


swap :: (a,b) %1 -> (b,a)
swap (x,y) = (y,x)


swap' :: (a,b) -> (b,a)
swap' (x,y) = (y,x)

swap'' :: (a,b) %m -> (b,a)
swap'' (x,y) = (y,x)


swapswap :: (a,b) %1 -> (a,b)
swapswap p = swap' (swap p)

swapswap' :: (a,b) -> (a,b)
swapswap' p = swap' (swap' p)

swapswap'' :: (a,b) %m -> (a,b)
swapswap'' p = swap'' (swap'' p)



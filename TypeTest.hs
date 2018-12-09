{-# LANGUAGE TypeOperators #-}

module TypeTest where


type a <~ b = b -> a


test
  :: Int
  <~ Int 
  <~ Int
test = (+)


(.~.)
  :: (c <~ a)
  <~ (b <~ a)
  <~ (c <~ b)
(.~.) = (.)


pm = (*2) .~. (+1)


s = show .~. (+1)

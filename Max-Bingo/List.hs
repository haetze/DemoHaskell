module List where


isPrefix::Eq a => [a] -> [a] -> Bool
isPrefix []     _      = True
isPrefix _      []     = False 
isPrefix (a:as) (b:bs)
  | a == b             = isPrefix as bs
  | otherwise          = False 


isSuffix as bs = isPrefix (reverse as) (reverse bs)


(<.>):: Monad m => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g = \a -> do
  b <- g a
  return $ f b

filter':: Monad m => (a -> m Bool) -> [a] -> m [a]
filter' _ [] = return []
filter' p (x:xs) = do
  px <- p x
  pxs <- filter' p xs
  if px then return (x:pxs) else return pxs

module MyState where

newtype State state describer = State { runState :: state -> (describer, state) }


exampleFunction:: Int -> (String, Int)
exampleFunction x | x <  0 = ("plus one", x + 1  )
                  | x == 0 = ("zero"    , 0      )
                  | x >  0 = ("div two" , div x 2)

stateExample = State exampleFunction

instance Functor (State state) where
  fmap f m  = State $ \st -> let (a, st') = runState m st
                                 b = f a
                             in (b, st') 
    

instance Applicative (State state) where
  pure x = State $ \s -> (x, s)
  m <*> m' = State $ \st -> let (a2b, st') = runState m st
                                (a, st'')  = runState m' st'
                            in (a2b a, st'')


instance Monad (State state) where
  return a = pure a
  m >>= k = State $ \s -> let (a, s') = runState m s
                          in runState (k a) s'


--push:: State [a] ()
push a = State $ \as -> ((), a:as)

pop:: State [a] a
pop = State $ \as -> case as of
                       [] -> error "Empty Stack"
                       (b:bs) -> (b, bs)

add:: State [Int] Int
add = do
  a <- pop
  b <- pop
  push $ a + b
  pop

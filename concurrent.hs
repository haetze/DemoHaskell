import Control.Concurrent

s f = do
	i <- forkIO $ f "sss\n"
	print i
	

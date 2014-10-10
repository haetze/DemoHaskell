import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char(isSpace)
import Control.Parallel.Strategies as P

parseLine:: [L.ByteString] -> [L.ByteString]
parseLine x =map findBody x

findBody:: L.ByteString -> L.ByteString
findBody x = if isSMS x 
	then  getBody  (L.split '=' x)
	else L.pack "" 

isSMS:: L.ByteString -> Bool
isSMS s = L.isPrefixOf (L.pack "<sms") (L.dropWhile isSpace  s)

body:: L.ByteString -> [L.ByteString]
body x = parseLine ( L.lines x)


getBody:: [L.ByteString] -> L.ByteString
getBody x | length x >= 5 = 6 <> x
getBody x = L.pack ""

formPath path = do
	con <- L.readFile path
	let s  = map L.unpack (body con)
	print s


(<>) :: Int -> [L.ByteString] -> L.ByteString
_ <> [] = L.pack ""	
0 <> x = head x
a <> x = (a-1) <> (tail x)

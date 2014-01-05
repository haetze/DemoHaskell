splitlines :: String -> [String]

splitlines [] = []
splitlines cs = 
	let (pre, suf) = break isLineTerminator cs
	in pre : case suf of
		('\r' :'\n' :rest) -> splitlines rest
		('\r' :rest) -> splitlines rest
		('\n':rest) -> splitlines rest
		_ -> []

isLineTerminator c = c == '\r' || c == '\n'


fixLines :: String -> String
fixLines input = unlines (splitLines input)

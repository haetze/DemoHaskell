--file: demo/WC.hs
--lines beginning with -- are comments

main = interact wordcount
	where wordcount input = show (length( lines input))++"\n"

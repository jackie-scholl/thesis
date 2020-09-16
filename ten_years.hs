module Ten_Years where

parseAndAdd :: String -> Integer
parseAndAdd age = (+10) (read age)

main :: IO ()
main = do
		putStr "How old are you? "
		ageString <- getLine
		let ageFuture = parseAndAdd ageString
		putStr "In ten years you will be: "  
		print ageFuture

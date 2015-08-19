palindrome :: String -> Maybe String
palindrome [] = Nothing
palindrome x = if x == (reverse x)
			   then Just x
			   else Nothing
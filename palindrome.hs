import qualified Data.Text as T
import Data.Maybe

palindrome :: String -> Maybe String
palindrome [] = Nothing
palindrome x = if x == (reverse x)
			   then Just x
			   else Nothing

countPalindromes :: String -> Int
countPalindromes x = (length 
					 (filter isJust 
					   (map palindrome 
					   	 (map T.unpack 
					   	   (T.tails 
					   	   	 (T.pack x)))))) -1 + (length x)


-- map palindrome (map T.unpack (T.tails (T.pack "12345")))
-- [Nothing,Nothing,Nothing,Nothing,Just "5",Nothing]

-- import Data.Maybe
-- filter isJust [Nothing,Nothing,Nothing,Nothing,Just "5",Nothing]
-- [Just "5"]


-- *Main T Data.Maybe> (filter isJust (map palindrome (map T.unpack (T.tails (T.pack "ana")))))



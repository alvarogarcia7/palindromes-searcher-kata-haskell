import qualified Data.Text as T
import Data.Maybe

palindrome :: String -> Maybe String
palindrome [] = Nothing
palindrome x = if x == (reverse x)
			   then Just x
			   else Nothing

countPalindromes :: String -> Int
countPalindromes x = length (generatePalindromes x)

generatePalindromes :: String -> [String]
generatePalindromes x = (catMaybes 
					   (map palindrome 
					   	 (generateCandidates x)))

generateCandidates :: String -> [String]
generateCandidates x = (map T.unpack
					     (map 
					   	   (\l -> substring l $ T.pack x) (substrings x)))

substrings :: String -> [[Int]]
substrings x = [[i,j] | i<- [0.. length x], j<-[i..(length x) - 1]]

substring :: [Int] -> T.Text -> T.Text
substring [start, end] text =  T.take (end - start + 1 ) $ T.drop start $ text



-- map palindrome (map T.unpack (T.tails (T.pack "12345")))
-- [Nothing,Nothing,Nothing,Nothing,Just "5",Nothing]

-- import Data.Maybe
-- filter isJust [Nothing,Nothing,Nothing,Nothing,Just "5",Nothing]
-- [Just "5"]


-- *Main T Data.Maybe> (filter isJust (map palindrome (map T.unpack (T.tails (T.pack "ana")))))



import Test.QuickCheck
import Test.QuickCheck.All

funcStr :: Gen String
funcStr = listOf $ elements "fgh"

identifierStr :: Gen String
identifierStr = resize 3 $ listOf $ elements "abcde"

correctLength :: String -> Bool
correctLength str = (length str) == 3

ordGen = oneof ["<", "==", ">"]

test = generate (suchThat funcStr correctLength )
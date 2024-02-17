module ReverseString (reverseString) where

reverseString :: String -> String
reverseString str =
  rev str
  where rev [] = []
        rev (x:xs) = reverse xs ++ [x]


-- If I want to change the function signature
reverseStringAlt :: String -> String
reverseStringAlt [] = []
reverseStringAlt (x:xs) = reverseString xs ++ [x]

-- Or I can create a secondary function and call it
reverseStringPri :: String -> String
reverseStringPri = reverseStringAlt

-- Or I can use the built-in function
reverseStringBuiltIn :: String -> String
reverseStringBuiltIn = reverse
import Data.Char

cipher ::  [Char] -> Int -> [Char]

cipher [] offset = []
cipher input offset = getCharAtIndexInAlphabet (getValidOffset (addOffsetToCharIndex (getCharIndexInAlphabet (head input)) offset)) : cipher (tail input) offset

getCharIndexInAlphabet :: Char -> Int
getCharIndexInAlphabet x = ord x - ord 'a'

addOffsetToCharIndex :: Int -> Int -> Int
addOffsetToCharIndex charindex offset = charindex + offset

getValidOffset :: Int -> Int
getValidOffset offset = mod offset ((ord 'z') - (ord 'a') + 1)

getCharAtIndexInAlphabet :: Int -> Char
getCharAtIndexInAlphabet index = chr (ord 'a' + index)
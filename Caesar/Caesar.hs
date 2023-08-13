-- Caesar's Cypher and its derrivatives

module Caesar where
import Data.Char

-- returns the next character. if z, loops back around to a
returnNext :: Char -> Char
returnNext c
  | c == 'Z'= 'A'
  | c == 'z'= 'a'
  | isAlpha c= chr (ord c + 1)
  | otherwise= c

-- does returnNext shift number of times
-- should be O(k) with k being the number of letters in the alphabet
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | shift == 0= c
  | otherwise= shiftChar (shift - 1) (returnNext c)

encodeCaesar :: [Char] -> Int -> [Char]
encodeCaesar message shiftRaw= let shift= mod shiftRaw 26 in
                                map (shiftChar shift) message

decodeCaesar :: [Char] -> Int -> [Char]
decodeCaesar message shiftRaw= let shift= 26 - mod shiftRaw 26 in
                                 map (shiftChar shift) message

-- returns the position of a letter in the alphabet
diffChar :: Char -> Int
diffChar c
  | isUpper c= ord c - ord 'A'
  | isLower c= ord c - ord 'a'
  | otherwise= 0

-- shifts the char by shift according to Caesar's Cypher.
-- should always be O(1) regardless of alphabet used
shiftChar' :: Int -> Char -> Char
shiftChar' shift c
  | isUpper c= chr (ord 'A' + mod (diffChar c + shift) 26)
  | isLower c= chr (ord 'a' + mod (diffChar c + shift) 26)
  | otherwise= c

encodeCaesar' :: [Char] -> Int -> [Char]
encodeCaesar' message shiftRaw= let shift= mod shiftRaw 26 in
                                map (shiftChar' shift) message

decodeCaesar' :: [Char] -> Int -> [Char]
decodeCaesar' message shiftRaw= let shift= 26 - mod shiftRaw 26 in
                                 map (shiftChar' shift) message


-- Like Caesar Cypher, but only a single shift, and Z is replaced with AA
encodeAugustus :: [Char] -> [Char]
encodeAugustus message= let caesar= encodeCaesar message 1 in
           foldl (\acc c -> if c == 'A'
                              then acc ++ "AA"
                            else if c == 'a'
                              then acc ++ "aa"
                            else acc ++ [c]) "" caesar

skipCount :: [Char] -> [Char] -> [Char]
skipCount acc []= acc
skipCount acc message= let h:t= message in
                       if h == 'Z' || h == 'z'
                         then skipCount (acc ++ [h]) (tail t)
                       else skipCount (acc ++ [h]) t

decodeAugustus :: [Char] -> [Char]
decodeAugustus message= let caesar= decodeCaesar message 1 in
                        skipCount [] caesar


extendCypher :: [Char] -> [Char] -> [Char]
extendCypher message cypher= if length cypher >= length message
                               then cypher
                             else extendCypher message (cypher ++ cypher)

matchCypher :: [Char] -> [Char] -> [Int]
matchCypher message cypher= let longCypher= extendCypher message cypher in
                            let rightCypher= take (length message) longCypher in
                            map diffChar rightCypher

vigenereEncodeHelper :: (Char, Int) -> Char
vigenereEncodeHelper (messageChar, shift)= head (encodeCaesar' [messageChar] shift)

encodeVigenere :: [Char] -> [Char] -> [Char]
encodeVigenere message cypher= let intCypher= matchCypher message cypher in
                               let pairings= zip message intCypher in
                               map vigenereEncodeHelper pairings

vigenereDecodeHelper :: (Char, Int) -> Char
vigenereDecodeHelper (messageChar, shift)= head (decodeCaesar' [messageChar] shift)

decodeVigenere :: [Char] -> [Char] -> [Char]
decodeVigenere message cypher= let intCypher= matchCypher message cypher in
                               let pairings= zip message intCypher in
                               map vigenereDecodeHelper pairings

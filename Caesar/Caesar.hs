-- Caesar's Cypher and its derivatives

module Caesar where
import Data.Char

import Utilities

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

-- encodes Caesar Cypher (Shift Cypher) on a String and inputted Int key
encodeCaesar :: [Char] -> Int -> [Char]
encodeCaesar message shiftRaw= let shift= mod shiftRaw 26 in
                                map (shiftChar shift) message

-- decodes Caesar Cypher (Shift Cypher) on a Cyphertext and inputted Int key
decodeCaesar :: [Char] -> Int -> [Char]
decodeCaesar message shiftRaw= let shift= 26 - mod shiftRaw 26 in
                                 map (shiftChar shift) message

-- returns the position of a letter in the alphabet
diffChar :: Char -> Int
diffChar c
  | isUpper c= ord c - ord 'A'
  | isLower c= ord c - ord 'a'
  | otherwise= 0

-- inverse of diffChar. Takes a letter position as an int and converts it into
-- a character
invDiffChar :: Int -> Char
invDiffChar n= chr (ord 'a' + n)

-- shifts the char by shift according to Caesar's Cypher.
-- should always be O(1) regardless of alphabet used
shiftChar' :: Int -> Char -> Char
shiftChar' shift c
  | isUpper c= chr (ord 'A' + mod (diffChar c + shift) 26)
  | isLower c= chr (ord 'a' + mod (diffChar c + shift) 26)
  | otherwise= c

-- a faster Caesar's Cypher
encodeCaesar' :: [Char] -> Int -> [Char]
encodeCaesar' message shiftRaw= let shift= mod shiftRaw 26 in
                                map (shiftChar' shift) message

-- a faster decode Caesar's Cypher
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

-- Takes an accumulator and a message. If it encounters a z, then it will skip
-- the next letter and continue
skipCount :: [Char] -> [Char] -> [Char]
skipCount acc []= acc
skipCount acc message= let h:t= message in
                       if h == 'Z' || h == 'z'
                         then skipCount (acc ++ [h]) (tail t)
                       else skipCount (acc ++ [h]) t

-- Decodes the Augustus Cypher
decodeAugustus :: [Char] -> [Char]
decodeAugustus message= let caesar= decodeCaesar message 1 in
                        skipCount [] caesar


-- Extends a string to match or exceed the length of another inputted string
extendCypher :: [Char] -> [Char] -> [Char]
extendCypher message cypher= if length cypher >= length message
                               then cypher
                             else extendCypher message (cypher ++ cypher)

-- Matches the length of two inputted strings
matchCypher :: [Char] -> [Char] -> [Int]
matchCypher message cypher= let longCypher= extendCypher message cypher in
                            let rightCypher= take (length message) longCypher in
                            map diffChar rightCypher

-- runs the encodeCaesar part of the Vigenere Cypher
vigenereEncodeHelper :: (Char, Int) -> Char
vigenereEncodeHelper (messageChar, shift)= head (encodeCaesar' [messageChar] shift)

-- encodes the Vigenere Cypher
encodeVigenere :: [Char] -> [Char] -> [Char]
encodeVigenere message cypher= let intCypher= matchCypher message cypher in
                               let pairings= zip message intCypher in
                               map vigenereEncodeHelper pairings

-- runs the Decode Caesar part of the Vigenere Cypher
vigenereDecodeHelper :: (Char, Int) -> Char
vigenereDecodeHelper (messageChar, shift)= head (decodeCaesar' [messageChar] shift)

-- decodes the Vigenere Cypher
decodeVigenere :: [Char] -> [Char] -> [Char]
decodeVigenere message cypher= let intCypher= matchCypher message cypher in
                               let pairings= zip message intCypher in
                               map vigenereDecodeHelper pairings

-- Performs the Affine function on a Char, and returns the encoded letter, or
-- if it is not a letter, it returns itself
affine :: Int -> Int -> Char -> Char
affine alpha beta c
  | isUpper c= let x= diffChar c in
               chr (ord 'A' + mod ((alpha * x) + beta) 26)
  | isLower c= let x= diffChar c in
               chr (ord 'a' + mod ((alpha * x) + beta) 26)
  | otherwise= c

-- Encodes the Affine Cypher
encodeAffine :: [Char] -> Int -> Int -> [Char]
encodeAffine message alpha beta= map (affine alpha beta) message

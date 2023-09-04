-- Caesar's Cypher and its derivatives

module Caesar where
import Data.Char

import Utilities

-- encodes Caesar Cypher (Shift Cypher) on a String and inputted Int key
encodeCaesar :: [Char] -> Int -> [Char]
encodeCaesar message shiftRaw=
  let shift= mod shiftRaw 26 in
  map (shiftChar shift) message

-- decodes Caesar Cypher (Shift Cypher) on a Cyphertext and inputted Int key
decodeCaesar :: [Char] -> Int -> [Char]
decodeCaesar message shiftRaw=
  let shift= 26 - mod shiftRaw 26 in
  map (shiftChar shift) message

-- a faster Caesar's Cypher
encodeCaesar' :: [Char] -> Int -> [Char]
encodeCaesar' message shiftRaw=
  let shift= mod shiftRaw 26 in
  map (shiftChar' shift) message

-- a faster decode Caesar's Cypher
decodeCaesar' :: [Char] -> Int -> [Char]
decodeCaesar' message shiftRaw=
  let shift= 26 - mod shiftRaw 26 in
  map (shiftChar' shift) message


-- Like Caesar Cypher, but only a single shift, and Z is replaced with AA
encodeAugustus :: [Char] -> [Char]
encodeAugustus message=
  let caesar= encodeCaesar message 1 in
  foldl (\acc c -> if c == 'A'
                     then acc ++ "AA"
                   else if c == 'a'
                     then acc ++ "aa"
                   else acc ++ [c]) "" caesar

-- Decodes the Augustus Cypher
decodeAugustus :: [Char] -> [Char]
decodeAugustus message=
  let caesar= decodeCaesar message 1 in
  skipCount [] caesar


-- encodes the Vigenere Cypher
encodeVigenere :: [Char] -> [Char] -> [Char]
encodeVigenere message cypher=
  let intCypher= matchCypher message cypher in
  let pairings= zip message intCypher in
  map (\(c, shift) -> head (encodeCaesar' [c] shift)) pairings

-- decodes the Vigenere Cypher
decodeVigenere :: [Char] -> [Char] -> [Char]
decodeVigenere message cypher=
  let intCypher= matchCypher message cypher in
  let pairings= zip message intCypher in
  map (\(c, shift) -> head (decodeCaesar' [c] shift)) pairings


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

-- Decodes the Affine Cypher
decodeAffine :: [Char ] -> Int -> Int -> [Char]
decodeAffine cypher alpha beta=
  let invAlpha= modInv alpha 26 in
  let invBeta= mod ((-invAlpha) * beta) 26 in
  map (affine invAlpha invBeta) cypher

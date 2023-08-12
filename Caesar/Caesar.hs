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
shiftChar :: Integer -> Char -> Char
shiftChar shift c
  | shift == 0= c
  | otherwise= shiftChar (shift - 1) (returnNext c)

encodeCaesar :: [Char] -> Integer -> [Char]
encodeCaesar message shiftRaw= let shift= mod shiftRaw 26 in
                                map (shiftChar shift) message

decodeCaesar :: [Char] -> Integer -> [Char]
decodeCaesar message shiftRaw= let shift= 26 - mod shiftRaw 26 in
                                 map (shiftChar shift) message

-- returns the position of a letter in the alphabet
diffChar :: Char -> Int
diffChar c
  | isUpper c= ord c - ord 'A'
  | isLower c= ord c - ord 'a'
  | otherwise= 0

-- shifts the char by shift according to Caesar's Cipher.
-- should always be O(1) regardless of alphabet used
shiftChar' :: Int -> Char -> Char
shiftChar' shift c
  | isUpper c= chr (ord 'A' + mod (diffChar c + shift) 26)
  | isLower c= chr (ord 'a' + mod (diffChar c + shift) 26)
  | otherwise= c

encodeCaesar' :: [Char] -> Int -> [Char]
encodeCaesar' message shiftRaw= let shift= mod shiftRaw 26 in
                                map (shiftChar' shift) message

-- Like Caesar Cipher, but only a single shift, and Z is replaced with AA
encodeAugustus :: [Char] -> [Char]
encodeAugustus message= let caesar= encodeCaesar message 1 in
           foldl (\acc c -> if c == 'A'
                              then acc ++ "AA"
                            else if c == 'a'
                              then acc ++ "aa"
                            else acc ++ [c]) "" caesar

decodeAugustus :: [Char] -> [Char]
decodeAugustus message= "UNIMPLEMENTED"
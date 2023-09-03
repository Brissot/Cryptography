module Utilities where
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

-- Takes an accumulator and a message. If it encounters a z, then it will skip
-- the next letter and continue
skipCount :: [Char] -> [Char] -> [Char]
skipCount acc []= acc
skipCount acc message=
  let h:t= message in
  if h == 'Z' || h == 'z'
    then skipCount (acc ++ [h]) (tail t)
  else skipCount (acc ++ [h]) t

-- Extends a string to match or exceed the length of another inputted string
extendCypher :: [Char] -> [Char] -> [Char]
extendCypher message cypher=
  if length cypher >= length message
    then cypher
  else extendCypher message (cypher ++ cypher)

-- Matches the length of two inputted strings
matchCypher :: [Char] -> [Char] -> [Int]
matchCypher message cypher=
  let longCypher= extendCypher message cypher in
  let rightCypher= take (length message) longCypher in
  map diffChar rightCypher

-- a helper function for coprime
coprimeHelper :: Int -> Int -> Int -> Int -> Bool
coprimeHelper alpha beta bigger factor
  | alpha < factor && beta < factor= True -- stop if factor becomes big
  | alpha `mod` factor == 0 && beta `mod` factor == 0= False
  | otherwise= coprimeHelper alpha beta bigger (factor + 1)

-- coprime is needed to check affine functions, since in modular arithmetic,
-- numbers are only guaranteed to have a unique multiplicative inverse if they
-- are coprime.
coprime :: Int -> Int -> Bool
coprime alpha beta= coprimeHelper alpha beta (max alpha beta) 2

-- a smaller coprime function
coprime' :: Int -> Int -> Bool
coprime' alpha n= gcd alpha n == 1

-- generates a list of all the integers from 0 to 25 that are coprime to 26
allAlphas :: Int -> [Int] -> [Int]
allAlphas alpha partial
  | alpha > 26= partial
  | otherwise= if coprime' alpha 26
                 then allAlphas (alpha + 1) (alpha:partial)
               else allAlphas (alpha + 1) partial

-- Generates a list from 0 to n
allBetas :: Int -> [Int]
allBetas n
  | n < 2= error "Beta must be >= 2"
  | otherwise= [0, 1.. (n - 1)]

-- Helper for modInv
modInvHelper :: Int -> Int -> Int -> Int
modInvHelper alpha n iter
  | alpha * iter `mod` n == 1= iter
  | otherwise= modInvHelper alpha n ((iter + alpha) `mod` n)

-- Calculates the modular arithmetic inverse of an inputted number and its mod
modInv :: Int -> Int -> Int
modInv alpha n= modInvHelper alpha n 1

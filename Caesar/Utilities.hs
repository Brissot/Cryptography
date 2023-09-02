module Utilities where

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

-- Calculates all pairs for which the modular arithmetic inverse is the same
-- as itself
sameAlpha :: Int -> [Int] -> [Int]
sameAlpha alpha partial
  | alpha > 26= partial
  | not (coprime' alpha 26)= sameAlpha (alpha + 1) partial
  | otherwise= let gamma= modInv alpha 26 in
               if gamma == alpha
                 then sameAlpha (alpha + 1) (gamma:partial)
               else sameAlpha (alpha + 1) partial

-- Calculates all pairs for which the modular arithmetic inverse is the same
-- as itself
sameBeta :: Int -> Int -> Int -> Int -> Bool
sameBeta alpha beta gamma n= mod beta n == mod (-gamma * beta) n

-- findSameBetas :: Int -> Int -> [(Int, Int)]
-- findSameBetas n alpha= let betas= allBetas n in
--                        let gamma= modInv alpha n in
--                        foldl (\acc beta -> if sameBeta alpha beta gamma n
--                                              then (alpha, beta):acc
--                                            else acc) [] betas

-- alphaBetaPairs :: [Int] -> Int -> [(Int, Int)]shiftChar' shift c
--   | isUpper c= chr (ord 'A' + mod (diffChar c + shift) 26)
--   | isLower c= chr (ord 'a' + mod (diffChar c + shift) 26)
--   | otherwise= c

-- alphaBetaPairs alphas n= foldl (findSameBetas n) [] 

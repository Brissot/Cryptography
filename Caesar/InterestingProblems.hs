module InterestingProblems where

import Data.Char
import Caesar
import Utilities

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

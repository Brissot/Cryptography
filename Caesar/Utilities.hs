module Utilites where

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
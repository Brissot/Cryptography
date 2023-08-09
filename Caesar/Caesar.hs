module Caesar where
import Data.Char

returnNext :: Char -> Char
returnNext 'Z'= 'A'
returnNext 'z'= 'a'
returnNext c= if (ord c >= ord 'A' && ord c <= ord 'Z')
              || (ord c >= ord 'a' && ord c <= ord 'z')
                then chr (ord c + 1)
                else c

shiftChar :: Integer -> Char -> Char
shiftChar shift c= if shift == 0
                     then c
                     else shiftChar (shift - 1) (returnNext c)

encodeCaesar :: [Char] -> Integer -> [Char]
encodeCaesar message shift_raw= let shift= mod shift_raw 26 in
                                map (shiftChar shift) message

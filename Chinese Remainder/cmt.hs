-- Fermat's little theorem is 
-- n**p = n**1 mod p which implies
-- n**(p-1) = n**0 mod p which implies
-- n**(p-2) = n**-1 mod p, which is the multiplicative modular inverse :)
fermat :: Integer -> Integer -> Integer
fermat n p= n^(p - 2) `mod` p
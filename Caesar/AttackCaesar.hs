-- For future attacks against Caesar's Cypher and its derivates using
-- frequency analysis and dictionaries

module Caesar where
import Data.Char

import Caesar

-- attacking Augustus' Cypher is trivial, because it is always a shift by 1
-- hence it only really works if the attacker does not know it's an Augustus
-- cypher
attackAugustus :: [Char] -> [Char]
attackAugustus= decodeAugustus

attackCaesar :: [Char] -> [Char]
attackCaesar encrypted= "UNIMPLEMENTED"

attackVigenere :: [Char] -> [Char]
attackVigenere encrypted= "UNIMPLEMENTED"

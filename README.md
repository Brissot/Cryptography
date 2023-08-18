Implementation of some encryption algorithms and some attacks against the
algorithms

## functions

**Caesar/Caesar.hs**

encodeAugustus message

decodeAugustus message

encodeCaesar message shift

decodeCaesar message shift

encodeCaesar' message shift (a much faster implementation)

decodeCaesar' message shift (a much faster implementation)

encodeVigenere message cypher

decodeVigenere message cypher

**Caesar/AttackCaesar.hs**

These are attacks, so it only needs the message, and from that it will try to
break the encryption. The difficulty of each attack ramps up from Augustus to
Caesar to Vigenere, which historically was thought to be unbreakable.

In truth, it was breakable even in its hayday. The Confederate States of
America, for instance used few keys, which allowed the union to consistantly
break the keys

attackAugustus encrypted

attackCaesar encrypted

attackVigenere encrypted

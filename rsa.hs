import Data.Bits

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m where t = if testBit e 0 then b `mod` m else 1


-- encryption
encr :: Integer -> Integer -> Integer -> Integer
encr phi e n = modExp phi e n

-- decryption
decr :: Integer -> Integer -> Integer -> Integer
decr c d n = modExp c d n


-- take heed, o eager being!
-- say, you know p and q and even n (oh wow!)
-- and you also know e ( /= 2.71828...)
-- and perhaps you also possess a secret message.
-- well done, now save all of this data on a disk.
-- ha
-- ok

-- find d such that mod e*d phi == 1

-- extended euclidean algorithm
-- a*x + b*y = 1
-- or
-- e*d + phi*y = 1
eea :: Integer -> Integer -> (Integer, Integer)
eea a 0 = (1, 0)
eea a b = (t, s - q * t) where (q, r) = quotRem a b
                               (s, t) = eea b r

mulInv :: Integer -> Integer -> Integer
mulInv e phi = fst $ eea e phi 


d = mod (mulInv e phi) phi





-- encryption
encr :: Integer -> Integer -> Integer -> Integer
encr phi e n = mod ( phi ^ e ) n

-- decryption
decr :: Integer -> Integer -> Integer -> Integer
decr c d n = mod ( c ^ d ) n


-- take heed, o eager being!
-- say, you know p and q and even n (oh wow!)
-- and you also know e ( /= 2.71828...)
-- and perhaps you also posses a secret message.
-- well done, now save all this data on a disk.
-- ha

-- find d such that mod ed phi == 1

-- extended euclidean algorithm
-- ax + by = 1
-- or
-- ed + phiy = 1
eea :: Integer -> Integer -> (Integer, Integer)
eea a 0 = (1, 0)
eea a b = (t, s - q * t) where (q, r) = quotRem a b
                               (s, t) = eea b r

mulInv :: Integer -> Integer -> Integer
mulInv e phi = fst $ eea e phi 


d = mod (mulInv e phi) phi



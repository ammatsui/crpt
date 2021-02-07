import Data.Char (chr, ord)
import Data.List as L
import Data.Function (on)

-- maybe I should make them newtypes to avoid mistakes
-- or not

type Hex = String

type Ascii = String

type Bin = [Int]

type Dec = Int

-- from hex 
hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'a' = 10
    | ch == 'b' = 11
    | ch == 'c' = 12
    | ch == 'd' = 13
    | ch == 'e' = 14
    | ch == 'f' = 15
    | otherwise = 145


parseHex :: Hex -> Dec
parseHex []       = 0
parseHex (x:y:xs) = 16 * hexChar x + hexChar y
parseHex _        = 145

fromHex :: Hex -> Ascii
fromHex s = map chr $ map parseHex (blocks s 2)

-- to hex
decHex :: Dec -> Hex
decHex = undefined

toHex :: Ascii -> Hex
toHex = undefined

-- from decimal to binary
helper :: Int -> [Int]
helper 0 = []
helper n = let (q,r) = n `divMod` 2 in r : helper q

toBin :: Dec -> Bin
toBin 0 = [0]
toBin n = reverse (helper n)

-- n  binary
toN :: Int -> [Int] -> [Int]
toN n xs = if length xs == n then xs else toN n (0 : xs) 

-- "join together"
join s = foldl (++) [] s

-- groups of n
blocks :: [a] -> Int -> [[a]]
blocks [] _ = []
blocks s  n = (take n s) : blocks (drop n s) n  

-- from binary to decimal
toDec :: Bin -> Dec
toDec []     = 0
toDec (x:xs) = x*2^i + toDec xs where i = length xs


strTo :: Ascii -> Bin
strTo s = join $ map (toN 8 . toBin) (map ord s)


fromBin :: Bin -> Ascii
fromBin xs = map chr $ map toDec (blocks xs 8)

-- xor
xor :: Int -> Int -> Int
xor 0 0 = 0 
xor 0 1 = 1
xor 1 0 = 1
xor 1 1 = 0
xor _ _ = 145

binXor :: Ascii -> Ascii -> Bin
binXor s t = zipWith xor (strTo s) (strTo t)

stringXor :: Ascii -> Ascii -> Ascii
stringXor s t = fromBin $ zipWith xor (strTo s) (strTo t)

-- encryption and decryption
encDec :: Ascii -> Ascii -> Ascii
encDec text key = stringXor (take (length text) (cycle key)) text


---------------------------------------------- single-byte xor cypher --------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
 
keys :: [String]
keys = foldr (\ n xs -> [chr n] : xs) [] (take 256 [0..])


-- evil laughter single byte
attack :: Hex -> [Ascii]
attack text = map (encDec (fromHex text)) keys

ok = ['A'..'Z'] ++ ['a'..'z'] ++ " " ++ ['1'..'9']++ "\"!.,\\?-'=;:%()_0" ++ ['А'..'Я'] ++ ['а'..'я'] ++ 'і'

isOk :: Ascii -> Bool
isOk s = all (\c -> elem c ok) s

modify :: Ascii -> Ascii
modify s = foldr (\c s -> case c of
                          '\"' -> "\\"++"\"" ++ s
                          '\'' -> "\\"++"\'" ++ s
                          '\\' -> "\\"++"\\" ++ s
                          otherwise -> c:s)           [] s


singleByte :: Hex -> [(Ascii, Ascii)]
singleByte encr = foldr (\mk res -> if isOk (fst mk) then (mk:res) else res) [] (zip list keys) where list = attack encr



-- let text = read ('"' : (modify encr) ++ "\"") :: String in let list = attack text in return $ foldr (\s ss -> if isOk (fst s) then s:ss else ss) [] (zip list keys)


----------------------------------------------- repeating key xor cypher -------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------- guessing the key length -----------------------------------------------------

-- cut the message into blocks of length n bits, for all n from 1 to (length of the message)

-- for each such partition, calculate the Hamming distance (divided by the length of a block) for any 2 blocks from that partition.

-- sum the values from the previous step and divide by m*(m-1), where m is the number of blocks in the partition.

-- choose the n with the smallest result for step 3.

-- Hamming distance is equal to the number of bits by which two strings of equal length differ.


toInt s = map (\n -> read n :: Int) (map show s)

-- all possible combinations
combs :: Int -> [a] -> [[a]]
combs _ []     = []
combs 0 _      = []
combs 1 x      = L.map (:[]) x
combs n (x:xs) = (L.map (x:) (combs (n-1) xs) ) ++ combs n xs   

binHam :: [Int] -> [Int] -> Int
binHam []     []     = 0
binHam x      []     = length x
binHam []     y      = length y
binHam (x:xs) (y:ys) = if x == y then (binHam xs ys) else 1 + (binHam xs ys)

-- normalised (by keysize) hamming distance (by the legnth of the shorter string)
nHamming :: String -> String -> Double
nHamming s t = (fromIntegral (binHam (strTo s) (strTo t)))/(fromIntegral $ min (length s) (length t))

-- returns list of normalised (by keysize) hamming distances for all possible pairs of blocks for a given keysize (which is the length of blocks)
allHam :: [String] -> [Double]
allHam ss = map (\[x, y] -> nHamming x y) (combs 2 ss)

-- calculates the average = sum of hamming distances / amount
avHam :: [String] -> Double
avHam []  = 0.0
avHam [x] = 0.0
avHam s   = 2* (sum (allHam s)) / (m * (m - 1))  where m = (fromIntegral (length s))

-- all possible key lengths (note greater than half of the length of the ciphertext)
keyLengths s = let n = floor $ (fromIntegral (length s))/2.0 in take n [1..] 

-- most likely key lenghts
-- note: the possible key length is the gcd of first 3 results
keyLen :: String -> [(Double, Int)]
keyLen s = L.sortBy (on compare fst) $ zip (map avHam (map (blocks s') (keyLengths s'))) [1..] where s' = fromHex s

----------------------------------------------- guess the key knowing its length ------------------------------------------

-- divide the ciphertext by blocks with equal length, same as the length of the key.

-- transpose the blocks. i.e., make a new block from the first bytes of the blocks, 
-- then a second block containing the second bytes of the blocks and so on…

-- each of the transposed blocks contains bytes that are encrypted with the same byte. 
-- this is the single-byte XOR cipher! And we already know how to break it.

-- crack the single-byte key for each of the transposed blocks.

-- all bytes taken together produce the key


-- n is the key length
-- list of the transposed blocks are attacked
-- result ascii

-- hex -> bin -> blocks of length n (bytes)

-- s          = 706c61696e74657874
-- blocks s 2 = 70 6c 61 69 6e 74 65 78 74
-- cut s 3    = [70 6c 61] [69 6e 74] [65 78 74]

cut :: Hex -> Int -> [[Hex]]
cut s n = blocks s' n where s' = blocks s 2

-- transpose $ cut s 3            = [70 69 65] [6c 6e 78] [61 74 74]
-- map join (transpose $ cut s 3) = [706965] [6c6e78] [617474] - each [...] - single-byte XOR cipher

kattack :: Hex -> Int -> [[(Ascii, Ascii)]]
kattack s n = map singleByte s' where s' = map join (transpose $ cut s n)


-- candidates for the corresponding position in the actual key
keysPos s n = map (\t -> map snd t) (kattack s n)




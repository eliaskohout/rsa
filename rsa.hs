import Data.Char


-- Auxiliary functions --

-- Checking if a number is prim
isPrim :: Integer -> Bool
isPrim 0 = False
isPrim n = 0 == (length 
    [ x | x <- [2..(truncate $ sqrt $ fromIntegral n)], n `mod` x == 0])

-- Multiples of a number
multiples :: Integer -> [Integer]
multiples 0 = [0]
multiples n = map (\x -> n * x) [1..]

-- Dividers of a number
dividers :: Integer -> [Integer]
dividers 0 = [1..]
dividers n = [ x | x <- [1..n], n `mod` x == 0 ]

-- Square and Mulitply (a^p mod m)
powInMod :: Integer -> Integer -> Integer -> Integer
powInMod a p m = sqrNMul 1 a p m
    where sqrNMul x y k n | k == 0         = x
                          | k `mod` 2 == 0 = sqrNMul x (y^2 `mod` n) (k `div` 2) n
                          | otherwise = sqrNMul ((x * y) `mod` n) ((y^2) `mod` n) (k `div` 2) n

-- String conversion
strToInt :: [Char] -> [Integer]
strToInt s = map (\x -> fromIntegral (ord x)) s

intToStr :: [Integer] -> [Char]
intToStr s = map (\x -> chr (fromIntegral x)) s


-- The Key -- 

-- The complete key K := (n, a, b, p, q)
data Key = Key Integer Integer Integer Integer Integer
    deriving Show

validateKey :: Key -> Bool
validateKey (Key n a b p q) =
       n == p * q
    && isPrim p
    && isPrim q
    && (a * b) `mod` ((p - 1) * (q - 1)) == 1
    -- The last statement is equal to '(a * b) mod φ(n) = 1' where φ is the
    -- euler function. It can be written as is because φ(n) = φ(p) * φ(q) and
    -- φ(r) = r - 1 for a prim number r.

generateKeyFromPrimes :: Integer -> Integer -> Key
generateKeyFromPrimes p q = (Key (p * q) a b p q)
    where b = atimesb `div` a
          a = last (init (dividers atimesb))
          atimesb = atimesbnotprims !! 0
          atimesbnotprims = filter (\x -> (length (dividers x) > 2)) atimesbs
          atimesbs = map (\x -> x + 1) (multiples ((p - 1) * (q - 1)))

-- The private part of K. d_K := (p, q, a)
data PrivateKey = PrivateKey Integer Integer Integer
    deriving Show

getPrivateKey :: Key -> PrivateKey
getPrivateKey (Key n a b p q) = (PrivateKey p q a)

-- The public part of K. e_K := (n, b)
data PublicKey = PublicKey Integer Integer
    deriving Show

getPublicKey :: Key -> PublicKey
getPublicKey (Key n a b p q) = (PublicKey n b)



-- Encryption and decryption -- 

-- Encrypt e(x) := x^b mod n
encryptInt :: PublicKey -> Integer -> Integer
encryptInt (PublicKey n b) x = powInMod x b n

-- Decrypt d(x) := x^a mod n 
decryptInt :: PrivateKey -> Integer -> Integer
decryptInt (PrivateKey p q a) x = powInMod x a (p * q)

-- Encrypt a string
encrypt :: PublicKey -> [Char] -> [Integer]
encrypt k l = map (\x -> encryptInt k x) (strToInt l)

-- Decrypt a string
decrypt :: PrivateKey -> [Integer] -> [Char]
decrypt k l = intToStr (map (\x -> decryptInt k x) l)

module Lesson_15 where

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where half = alphabetSize `div` 2
        offset = fromEnum c + half
        rotation = offset `mod` alphabetSize

rotChar :: Char -> Char
rotChar = rotN sizeOfAlphabet
  where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                  then fromEnum c + halfN
                  else fromEnum c + halfN + 1
        rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder alphaSize

-- ----------------------------------------------------

xorBool :: Bool -> Bool -> Bool
xorBool p q = (p || q) && (not (p && q))

xorPair :: (Bool, Bool) -> Bool
xorPair (p, q) = xorBool p q

xor :: [Bool] -> [Bool] -> [Bool]
xor ps qp = map xorPair (zip ps qp)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (reminder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where reminder = n `mod` 2
        nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits = maxBits - (length reversedBits)
        leadingFalses = replicate missingBits False

charToBits :: Char -> [Bool]
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\(_, i) -> 2^i) trueLocations)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\(v, _) -> v) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\(p, t) -> p `xor` t)
                          (zip padBits plainTextBits)
  where padBits = map charToBits pad
        plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bits
  where bits = applyOTP' pad plainText

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad


class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot
instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

data OneTimePad = OTP String
instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

data StreamCipher = SC Int
instance Cipher StreamCipher where
  encode (SC seed) text = applySC seed text
  decode (SC seed) text = applySC seed text

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

randomStream :: Int -> [Int]
randomStream seed = nextValue : randomStream nextValue
  where nextValue = prng 1410 7 100 seed

applySC' :: Int -> String -> [Bits]
applySC' seed text = map (\(p, q) -> p `xor` q)
                    (zip textBits randomBits)
  where textBits = map charToBits text
        randomBits = map intToBits (randomStream seed)

applySC :: Int -> String -> String
applySC seed text = map bitsToChar encodedBits
  where encodedBits = applySC' seed text

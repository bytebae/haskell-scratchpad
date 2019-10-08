{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List as L
import Data.Time
import Data.List (intersperse)


nonsense :: Bool -> Integer
nonsense True = 131
nonsense False = 666

curriedF :: Integer -> Bool -> Integer
curriedF i b = i + (nonsense b)

uncurriedF :: (Integer, Bool) -> Integer
uncurriedF (i, b) = i + (nonsense b)

anonF :: Integer -> Bool -> Integer
anonF = \i b -> i + (nonsense b)

anonNestedF :: Integer -> Bool -> Integer
anonNestedF = \i -> \b -> i + (nonsense b)

validFunc :: (Num a, Fractional a) => [a] -> a -> a
validFunc ls num = num / fromIntegral (length ls)

f :: Int -> Bool
f 1 = True
f _ = False

-- Eq typeclass excercises

data DayOfWeek = Mon | Tue | Wed |Thu | Fri | Sat | Sun deriving (Show, Eq)

data Date = Date DayOfWeek Int deriving Show

{- instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sun Sun = True
  (==) _ _     = False -}

-- -- custom ordering for DayOfWeek s.t. Friday is max and all else EQ
instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ

instance Eq Date where
  (==) (Date dow dom) (Date dow' dom') = (dow' == dow) && (dom' == dom)

data Identity a =
  Identity a deriving Show

data Ldent a = Ldent a deriving (Show, Eq, Ord)

-- TODO: Remember this!!!
instance (Eq a) => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

instance (Ord a) => Ord (Identity a) where
  (>) (Identity v) (Identity v') = v > v'
  (<) (Identity v) (Identity v') = v < v'
  (>=) (Identity v) (Identity v') = v >= v'
  (<=) (Identity v) (Identity v') = v <= v'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn v) (TisAn v') = v == v'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two v1 v2) (Two v1' v2') = (v1 == v1') && (v2 == v2')

data StringOrInt =
  TisAnInt Int
  |TisAString String

-- TODO: make this error properly when comparing TisAnInt and TisAString
instance Eq StringOrInt where
  (==) (TisAnInt v) (TisAnInt v') = v == v'
  (==) (TisAString s) (TisAString s') = s == s'

data PairM a = Pair a a

data PairInt = PairIntVal Int Int

instance Eq a => Eq (PairM a) where
  (==) (Pair v v') (Pair b b') = v == b

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (a == a') && (b == b')

data Which a =
  ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'

data EitherOr a b =
  Hello a | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a') (Hello b') = a' == b'
  (==) (GoodBye a') (GoodBye b') = a' == b'

-- Num typeclass

check' :: Ord a => a -> a -> Bool
check' a a' = a == a'

-- -- self problem: create stringSucc which
-- -- increments the chars of a string from LSB to MSB
{-
stringSucc :: String -> String
stringSucc s =  -}

-- example of bad things... ** TODO: understand this better.

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

-- NOTE: 'newtype` allows "deriving" typeclasses
--       `type` does not allow the same syntax
newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed where
  integerOfA = toNumber a
  integerOfAPrime = toNumber a'
  summed = integerOfA + integerOfAPrime

  -- example of worse things... ** TODO: understand this better.

class NumberishWorse a where
  fromNumberWorse      :: Integer -> a
  toNumberWorse        :: a -> Integer
  defaultNumberWorse   :: a

instance NumberishWorse Age where
  fromNumberWorse n      = Age n
  toNumberWorse (Age n)  = n
  defaultNumberWorse     = Age 55

instance NumberishWorse Year where
  fromNumberWorse n      = Year n
  toNumberWorse (Year n) = n
  defaultNumberWorse     = Year 2011

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood1 = Blah1 | Woot1 deriving (Show, Eq)

-- settleDown :: Eq Mood1 => Mood1 -> Mood1
settleDown x = if x == Woot1 then Blah1 else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Show, Eq)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Jooly" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

i :: Num a => a
i = 1

freund :: a -> a
freund x = x

sigmond :: Num a => a -> a
sigmond x = 1


jung :: [Int] -> Int
jung xs = head $ L.sort xs

-- page: 212 - haskell programming from first principles
-- TODO: create a fn with the following def:
--    chk :: Eq b => (a -> b) -> a -> b -> Bool
--    chk = ???

-- TODO: create a fn with the following def:
--    arith :: Num b => (a -> b) -> Integer -> a -> b
--    arith = ???
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.

newtype Nada = Nada Double deriving (Eq, Show)

instance Num Nada where
  (+) (Nada x) (Nada y) = Nada (x + y)
  (-) (Nada x) (Nada y) = Nada (x - y)
  (*) (Nada x) (Nada y) = Nada (x * y)
  signum (Nada x) = Nada (signum x)
  fromInteger (x) = Nada (fromInteger x)
  abs (Nada x) = Nada (abs x)

instance Fractional Nada where
  (Nada x) / (Nada y)         = Nada (x / y)
  recip (Nada n)              = Nada (recip n)
  fromRational r              = Nada (fromRational r)

bindExp :: Integer -> String
bindExp x = let y =10; z = y + x in
            "the int was: " ++ show x ++
            " and y was: " ++ show y ++
            " and z was: " ++ show z

bindExp2 :: Integer -> String
bindExp2 x = let x = 10; y = 5 in
  "x was: " ++ show x ++
  " and y was: " ++ show y

mTh1 x y z = x * y * z
mTh2 x y = \z -> x * y * z
mTh3 x = \y -> \z -> x * y * z
mTh4 = \x -> \y -> \z -> x * y * z

sampleAnonFunc :: Ord a => a -> a -> a
sampleAnonFunc = \x -> \y -> (if x > y then y else x)

mflipAnon f = \x -> \y -> f y x

mflipDeAnon f x y = f y x

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
            | RegisteredUser Username AccountNumber

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum))
        = putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive = Galapagos
                        | Antarctica
                        | Australia
                        deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

bloodNa :: Integer -> String
bloodNa x
        | x < 135 = "too low"
        | x > 145 = "too high"
        | otherwise = "just right"
dogYears :: Int -> Int
dogYears x
        | x <= 0      = 0
        | x <= 1      = x * 15
        | x <= 2      = x * 12
        | x <= 4      = x * 8
        | otherwise   = x * 6

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
        | a^2 + b^2 == c^2 = "Correctoo"
        | otherwise        = "Nope"

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
        | y >= 0.9 = 'A'
        | y >= 0.8 = 'B'
        | otherwise = 'F'
        where y = x / 100

pal xs
      | xs == reverse xs = True
      | otherwise        = False

tensDigit :: Integral a => a -> a
tensDigit x = d
        where xLast = x `div` 10
              d     = xLast `mod` 10

tensDigit2 :: Integral a => a -> a
tensDigit2 x = (fst $ divMod x 10) `mod` 10

-- implement a check to see if argument > 99, i.e., has a hundreds digit
hundredsDigit :: Integral a => a -> a
hundredsDigit x = (fst $ divMod (fst $ divMod x 10) 10) `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool p1 p2 boolval
    | boolval == True = p1
    | otherwise       = p2

-- TODO --
gPage262 :: (a -> b) -> (a, c) -> (b, c)
gPage262 f1 (a, c) = result
  where result = ((f1 a),  c)
        f1 = undefined

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = \x -> read (show x)

-- Your task now is to change the type of roundTrip in Arith4 to (Show
-- a, Read b) => a -> b. How might we tell GHC which instance of
-- Read to dispatch against the String now? Make the expression
-- print (roundTrip 4) work. You will only need the has the type
-- syntax of :: and parentheses for scoping.

-- TODO --
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' x = undefined
-- roundTrip' x = d where
--   c = read (show x)
--   d' = show c
--   d = undefined

definitelyDontDoThis :: Bool -> Int
definitelyDontDoThis True = 1
definitelyDontDoThis False = error "ooops hehehe" -- ALSO never use error

-- Recursion Chapter p274

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times-1) n)

-- applyTimes is a fn that applies another fn (b -> b), a number of times
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = f b
applyTimes n f b = f (applyTimes (n-1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes  times (+1) n

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes' (n-1) f $ b

-- bottom p280

fff :: Bool -> Maybe Int
fff False = Just 0
fff _     = Nothing

-- fib :: Integral a => a -> a
-- TODO: can performance be improved?
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-----------------------------------------------------------------
-- NOTE: observe and remmeber the following definition pattern --
-----------------------------------------------------------------
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = divideIt n d 0
  where divideIt nu de i
          | nu < de   = (i, nu)
          | otherwise = divideIt (nu - de) de (i + 1)
-----------------------------------------------------------------

sumOneToN :: (Eq a, Num a) => a -> a
sumOneToN 0 = 0
sumOneToN 1 = 1
sumOneToN n = n + sumOneToN (n - 1)

-- Write a function that multiplies two integral numbers using
-- recursive summation. The type should be (Integral a) => a ->
-- a -> a.
-- p293 Q3.

multRecSum :: Integral a => a -> a -> a
multRecSum n m
  | m == 1        = n
  | otherwise     = n + (multRecSum n (m - 1))


-- *** TODO ***

-- modify dividedBy above to handle 0 and negative divisors

-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy n d = divideIt n d 0
--   where divideIt nu de i
--           | nu < de   = (i, nu)
--           | otherwise = divideIt (nu - de) de (i + 1)


-- The McCarthy 91 function yields x − 10 when x > 100 and 91
-- otherwise. The function is recursive.

mc91 = undefined

-- mc91 :: Integer -> Integer
-- mc91 x
--   | x > 100       = x - 10
--   | otherwise     = 91
-- TODO: make recursive as per p294

-- Numbers to words
-- We’ve laid out multiple functions for you to consider as you tackle
-- the problem. You may not need all of them, depending on how you
-- solve it–these are just suggestions. Play with them and look up their
-- documentation to understand them in deeper detail.
-- You will probably find this difficult.
-- div :: Integral a => a -> a -> a
-- mod :: Integral a => a -> a -> a
-- map :: (a -> b) -> [a] -> [b]
-- concat :: [[a]] -> [a]
-- intersperse :: a -> [a] -> [a]
-- (++) :: [a] -> [a] -> [a]
-- (:[]) :: a -> [a]
-- Also consider:
-- Prelude> div 135 10
-- 13
-- Prelude> mod 135 10
-- 5
-- Prelude> div 13 10
-- 1
-- Prelude> mod 13 10
-- 3

-- Here is what your output should look in the REPL when it’s work-
-- ing:
-- Prelude> wordNumber 12324546
-- "one-two-three-two-four-five-four-six"


-- import Data.List (intersperse) done BOF

digitToWord :: Int -> String
digitToWord n = undefined

digits :: Int -> [Int]
digits n = undefined

wordNumber :: Int -> String
wordNumber n = undefined

--

safeHead :: [a] -> Maybe a
safeHead []       = Nothing
safeHead (x:[])   = Just x
safeHead (x: xs)  = Just x

safeTail :: [a] -> Maybe [a]
safeTail []       = Nothing
safeTail (x:[])   = Nothing
safeTail (x: xs)  = Just xs

-- Write your own enumFromTo definitions for the types provided. Do
-- not use range syntax to do so. It should return the same results as if
-- you did [start..stop].

-- eftBool :: Bool -> Bool -> [Bool]
-- eftBool = undefined

-- eftOrd :: Ordering -> Ordering -> [Ordering]
-- eftOrd = undefined

-- eftInt :: Int -> Int -> [Int]
-- eftInt = undefined

-- eftChar :: Char -> Char -> [Char]
-- eftChar = undefined
---

-- p308
-- Using takeWhile and dropWhile, write a function that takes a string
-- and returns a list of strings, using spaces to separate the elements
-- of the string into words, as in the following sample:

-- *Main> myWords "all i wanna do is have some fun"
-- ["all","i","wanna","do","is","have","some","fun"]

myWords :: String -> [String]
myWords ""  = []
myWords inputS = resultList
  where
    word = takeWhile (/=' ') $ dropWhile (==' ') inputS
    lPos = length word
    (_, restS) = splitAt lPos inputS
    resultList = [word] ++ myWords (dropWhile (/=' ') restS)
-- derived from: https://stackoverflow.com/questions/53461230/how-would-i-split-a-string-after-the-spaces-in-haskell

-- recursiveTraverse :: [Char] -> String
-- recursiveTraverse [] = []
-- recursiveTraverse (x:xs)
--   | x /= ' ' = [x] ++ (recursiveTraverse xs)
--   | x == ' ' = recursiveTraverse xs


-- List Comprehensions

-- NOTE: this beauty
acro xs = [x | x <- xs, elem x ['A'..'Z']]

-- Spines and non-strictness

-- We can use a special command in GHCi called sprint to print vari-
-- ables and see what has been evaluated already, with the underscore
-- representing expressions that haven’t been evaluated yet.
-- WARNING: :sprint has some behavioral quirks that can be a bit
-- frustrating.

-- Spines are evaluated independently of values
-- Values in Haskell get reduced to weak head normal form by default.
-- By ‘normal form’ we mean that the expression is fully evaluated.
-- ‘Weak head normal form’ means the expression is only evaluated as
-- far as is necessary to reach a data constructor.
-- Weak head normal form (WHNF) is a larger set and contains both
-- the possibility that the expression is fully evaluated (normal form)
-- and the possibility that the expression has been evaluated to the point
-- of arriving at a data constructor or lambda awaiting an argument.
-- For an expression in weak head normal form, further evaluation
-- may be possible once another argument is provided. If no further
-- inputs are possible, then it is still in WHNF but also in normal form
-- (NF).

-----
-- NOTE:
--
-- \x -> x * 10 -- WHNF & NF
-- This anonymous function is in normal form because while (*)
-- has been applied to two arguments of a sort, it cannot be reduced
-- further until the outer x -> ... has been applied. With nothing
-- further to reduce, it is in normal form.

-- Remember that an expression cannot be in normal form or weak
-- head normal form if the outermost part of the expression isn’t a data
-- constructor. It can’t be in normal form if any part of the expression
-- is unevaluated.
-----

-- Write your own version of zip :: [a] -> [b] -> [(a, b)] and
-- ensure it behaves the same as the original.

ownZip :: [a] -> [b] -> [(a, b)]
ownZip [] _ = []
ownZip _ [] = []
ownZip (a:as) (b:bs) = [(a, b)] ++ ownZip as bs

--
-- Write your own version of zipWith

ownZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
ownZipWith f [] _ = []
ownZipWith f _ [] = []
ownZipWith f (a:as) (b:bs) = [f a b] ++ ownZipWith f as bs

-- Rewrite your zip in terms of the zipWith you wrote.

ownZip' :: [a] -> [b] -> [(a, b)]
ownZip' a b = ownZipWith (,) a b

-- requires Data.Char

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = [toUpper x] ++ xs

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = [toUpper x] ++ capitalizeAll xs


--- Casesar cipher with max displacement 26
-- alphabetsLower = ['a'..'z']
-- alphabetsLower =
-- alphabetsUpper = map toUpper alphabetsLower

-- Above approach is replaced with ord, chr (from data.Char)
-- and mod (standard?)

caesarCipher :: Int -> String -> String
caesarCipher _ [] = []
caesarCipher shiftAmount (s:ss)
  | isLower s = [chr ((mod (ord s - ord 'a' + shiftAmount) 26) + ord 'a')] ++
                (caesarCipher shiftAmount ss)
  | otherwise = [chr ((mod (ord s - ord 'A' + shiftAmount) 26) + ord 'A')] ++
                (caesarCipher shiftAmount ss)

-- TODO: include an unCaesar function
uncaesarCipher :: Int -> String -> String
uncaesarCipher _ [] = []
uncaesarCipher shiftAmount (s:ss)
  | isLower s       = [chr ((mod (ord s - ord 'a' - shiftAmount) 26) + ord 'a')] ++
                      (uncaesarCipher shiftAmount ss)
  | otherwise       = [chr ((mod (ord s - ord 'A' - shiftAmount) 26) + ord 'A')] ++
                      (uncaesarCipher shiftAmount ss)

--- Folding lists

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b           -- <== This.
myFoldr f b (a:as) = f a (myFoldr f b as)

-- changing the type signature for kicks
myFoldr' :: (a -> b -> b) -> [a] -> b -> b
myFoldr' f [] z      = z
myFoldr' f (a:as) z   = f a (myFoldr' f as z)

someRange = map show [1..5]
foldrExample = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" someRange
-- shows right-associativity of foldr => "(1+(2+(3+(4+(5+0)))))"

---
-- Write an 'ANY' function that takes another function (a -> Bool
-- and a list [a], and returns the OR of all [f a]
--

myAny :: (a -> Bool) -> [] a -> Bool
myAny f as = foldr (\m n -> f m || n) False as

-- The ANY function without foldr -- note the explicit right-associativity
myAny' :: (a -> Bool) -> [] a -> Bool
myAny' _ []     = False
myAny' f (a:as) = (f a || False) || (myAny' f as) -- is it like python's OR?

---

--- NOTE: Consider this:
lll = length $ take 2 $ take 4 ([1, 2]++undefined)
-- 2
-- It doesn’t matter that take 4 could’ve hit the bottom! Nothing
-- forced it to because of the take 2 between it and length.
---

-- TODO: Revisit pg353 for foldr behaviour with undefined (aka bottom)
--       values in leaves

-- Prelude> foldr (\_ _ -> 9001) 0 [1..5]
-- 9001
-- Prelude> foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]
-- 9001
-- Prelude> foldr (\_ _ -> 9001) 0 ([1, 2, 3] ++ undefined)
-- 9001
-- Everything is fine unless the first piece of the spine is bottom:

-- Prelude> foldr (\_ _ -> 9001) 0 undefined
-- *** Exception: Prelude.undefined

-- Prelude> foldr (\_ _ -> 9001) 0 [1, undefined]
-- 9001
-- Prelude> foldr (\_ _ -> 9001) 0 [undefined, undefined]
-- 9001

-- The final two examples work because it isn’t the first cons cell
-- that is bottom — the undefined values are inside the cons cells, not
-- in the spine itself. Put differently, the cons cells contain bottom
-- values but are not themselves bottom.

---
-- Foldl (LEFT) (tail recursion)

myFoldl :: (b -> a -> b) -> b -> [] a -> b
myfoldl f acc []      = acc       -- <== This.
myFoldl f acc (a:as)  = myFoldl f (f acc a) as

-- contrast with foldr (RIGHT) (non-tail recursion)

myFoldr_ :: (a -> b -> b) -> b -> [a] -> b
myFoldr_ f acc []     = acc
myFoldr_ f acc (a:as) = f a (myFoldr f acc as)

-- NOTE: notice the difference in function application for foldl vs foldr
--        i.e., foldl = ...f acc a...
--              foldr = ...f acc as...
--
-- NOTE: An important difference between foldr and foldl is that a left fold
-- has the successive steps of the fold as its first argument.
-- The next recursion of the spine isn’t intermediated by the folding function
-- as it is in foldr, which also means recursion of the spine is unconditional.
---

someRange' = map show [1..5]

foldrExample' = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" someRange'
-- shows right-associativity of foldr => "(1+(2+(3+(4+(5+0)))))"

foldlExample' = foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" someRange'
-- shows left-associativity of foldl => "(((((0+1)+2)+3)+4)+5)"

-- Note:

-- The relationship between the scans and folds are as follows:
-- last (scanl f z xs) = foldl f z xs
-- head (scanr f z xs) = foldr f z xs

-- more contrast:

-- foldr (^) 2 [1..3]
-- (1 ^ (2 ^ (3 ^ 2)))
-- (1 ^ (2 ^ 9))
-- 1 ^ 512
-- 1

-- foldl (^) 2 [1..3]
-- ((2 ^ 1) ^ 2) ^ 3
-- (2 ^ 2) ^ 3
-- 4 ^ 3
-- 64

-- Also note this:

-- Prelude> foldr (:) [] [1..3]
-- [1,2,3]
--

-- [] (1 : 2 : 3 : []) == 1 : (2 : (3 : []))

-- Prelude> foldl (flip (:)) [] [1..3]
-- [3,2,1]
--
-- foldl f z [1, 2, 3]
-- f ~ (flip (:)); z ~ []
-- (((z `f` 1) `f` 2) `f` 3)

-- pg361 Q (e)
-- What is wrong with the following expression?
-- foldl ((++) . show) "" [1..5]

ans361_5E = foldl (++) "" $ map show [1..5]

funcPg364_R :: [String] -> String
funcPg364_R lx = foldr (\ a b -> take 4 a ++ b) "" lx

funcPg364_L :: [String] -> String
funcPg364_L lx = foldl (\ b a -> take 4 a ++ b) "" lx

-- NOTE:
-- If we want to be explicit, we can assert types for the values
-- consumed by the above lambda functions
-- This can be useful for checking that your mental model of the
-- code is accurate.

fR a b = take 4 (a::String) ++ (b::String)
fL a b = take 4 (b::String) ++ (a::String)

funcPg364_R' lx = foldr fR "" lx
funcPg364_L' lx = foldl fL "" lx

-- Excercises: Database Processing (for this Data.Time is imported)

data DatabaseItem       = DbString String
                        | DbNumber Integer
                        | DbDate UTCTime
                        deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9001
  , DbNumber 9001
  , DbString "hello db!"
  , DbString "hello again db!!!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- TODO: How does the following really work?
isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _          = False

isDbString :: DatabaseItem -> Bool
isDbString (DbString _) = True
isDbString _            = False

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _            = False

unpackDbDate :: DatabaseItem -> UTCTime
unpackDbDate (DbDate x) = x

unpackDbString :: DatabaseItem -> String
unpackDbString (DbString x) = x

unpackDbNumber :: DatabaseItem -> Integer
unpackDbNumber (DbNumber x) = x

-- Q1. Write a function that filters for DbDate values and returns a list
-- of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = map unpackDbDate $ filter isDbDate db

-- Q2. Write a function that filters for DbNumber values and returns a list
-- of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map unpackDbNumber $ filter isDbNumber db

-- Q3. 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db  = result
  where
    result     = foldr (max) headDate allDates
    headDate   = (head $ filterDbDate theDatabase)
    allDates   = map unpackDbDate $ filter isDbDate theDatabase

-- Q4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb db = sum $ filterDbNumber db

-- Q5. Write a function that gets the average of the DbNumber values.
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / (fromIntegral (length $ filterDbNumber db))
-- TODO: Is there a Better way to use fromIntegral above?

---

-- 10.7
-- pg368
-- Folding and evaluation
--
-- What differentiates foldr and foldl is associativity. The right as-
-- sociativity of foldr means the folding function evaluates from the
-- innermost cons cell to the outermost (the head). On the other hand,
-- foldl recurses unconditionally to the end of the list through self-calls
-- and then the folding function evaluates from the outermost cons
-- cell to the innermost:
--
-- Prelude> take 3 $ foldr (:) [] ([1, 2, 3] ++ undefined)
-- [1,2,3]
--
-- Prelude> take 3 $ foldl (flip (:))
-- *** Exception: Prelude.undefined

-- NOTE: Relationship between foldr and foldf for FINITE lists
--
-- foldr f z xs = foldl (flip f) z (reverse xs)
--
---

---
-- NOTE: Observe this recursive function that GENERATES the fib sequence
fibs = 1 : scanl (+) 1 fibs -- !!!

-- Return the nth fib number with the above function

fibN n = fibs !! n -- this is so good

---
-- pg373 q3
-- Try to write the factorial function from Recursion as a scan.
-- You’ll want scanl again, and your start value will be 1. Warning:
-- this will also generate an infinite list, so you may want to pass it
-- through a take function or similar.

factScanl :: Int -> Integer
factScanl n = (scanl (*) 1 [1..])!! n --1 : scanl (*) [1..]

-- pg375
myAndFold :: [Bool] -> Bool
myAndFold = foldr (&&) True

myOrFold :: [Bool] -> Bool
myOrFold = foldr (||) False

myAnyFold :: (a -> Bool) -> [a] -> Bool
-- myAnyFold f x = myOrFold $ map f x  -- not pointfree
myAnyFold = \f x -> myOrFold $ map f x -- pointfree version... can be improved?

-- Write two versions of myElem. One version should use folding
-- and the other should use any.
-- myElem :: Eq a => a -> [a] -> Bool
-- Prelude> myElem 1 [1..10]
-- True
-- Prelude> myElem 1 [2..10]
-- False

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold k l = myAnyFold (\x -> x == k) l

-- Implement myReverse, don’t worry about trying to make it
-- lazy.
-- myReverse :: [a] -> [a]
-- myReverse = undefined
-- Prelude> myReverse "blah"
-- "halb"
-- Prelude> myReverse [1..5]
-- [5,4,3,2,1]

myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = (myReverse xs) ++ [x]

-- 5. Write myMap in terms of foldr. It should have the same behavior
-- as the built-in map.
-- myMap :: (a -> b) -> [a] -> [b]

myMap :: (a -> b) -> [a] -> [b]
myMap f []      = []
-- myMap f (x:xs) = [f x] ++ (myMap f xs) -- construct same operations with foldr
myMap f (x:xs)  = foldr (++) [f x] [myMap f xs] -- expected expression

-- Write myFilter in terms of foldr. It should have the same behav-
-- ior as the built-in filter.
-- myFilter :: (a -> Bool) -> [a] -> [a]
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x: xs)
    | f x == True = [x] ++ myFilter f xs
    | otherwise   = myFilter f xs

-- 7. squish flattens a list of lists into a list
-- squish :: [[a]] -> [a]
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

-- 8. squishMap maps a function over a list and concatenates the re-
-- sults.
-- squishMap :: (a -> [b]) -> [a] -> [b]

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []      = []
squishMap f (x:xx)  = (f x) ++ (squishMap f xx)

-- 9. squishAgain flattens a list of lists into a list. This time re-use the
-- squishMap function.
-- squishAgain :: [[a]] -> [a]

squishAgain :: [[a]] -> [a]
squishAgain l = squishMap (concat) [l]    -- Why [l]??

-- 10. myMaximumBy takes a comparison function and a list and returns
-- the greatest element of the list based on the last value that the
-- comparison returned GT for.
-- myMaximumBy :: (a -> a -> Ordering) -> [a] -> a

myMaximumBy :: (Eq a, Ord a) => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs)
    | xs == []              = x
    | f x (head xs) == LT   = myMaximumBy f xs
    | otherwise             = myMaximumBy f (x: (tail xs))


-- 11. myMinimumBy takes a comparison function and a list and returns
-- the least element of the list based on the last value that the
-- comparison returned LT for.
-- myMinimumBy :: (a -> a -> Ordering) -> [a] -> a

myMinimumBy :: (Eq a, Ord a) => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs)
  | xs == []              = x
  | f x (head xs) == GT   = myMinimumBy f xs
  | otherwise             = myMinimumBy f (x: (tail xs))


--- Concludes the chapter on folding lists
-- REVISE summary @ pg379

-- SELF: write a function that maps a function over an iterable and returns [(result, enum)]
myEnumMap :: (a -> b) -> [a] -> [(b, Integer)]
myEnumMap f l = myEnumIndexMap f l 0
  where
    myEnumIndexMap f [] c     = []
    myEnumIndexMap f (l:ls) c = [((f l), c)] ++ myEnumIndexMap f ls (c+1)

-- Summary definitions

-- 1. A Fold is a higher-order function which, given a function to
-- accumulate the results and a recursive data structure, returns
-- the built up value. Usually a “start value” for the accumulation
-- is provided along with a function that can combine the type of
-- values in the data structure with the accumulation. The term
-- fold is typically used with reference to collections of values
-- referenced by a recursive datatype.

-- 2.
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)
-- Not tail recursive, we give up control to the combining function
-- f before continuing through the list. foldr’s recursive calls will
-- bounce between foldr and f.

-- foldl f z [] = z
-- foldl f z (x:xs) = foldl f (f z x) xs
-- Tail recursive. foldl invokes itself recursively. The combining
-- function is only an argument to the recursive fold.



--- Chapter: Algebraic datatypes


-- Let's begin with a review of the important parts of datatypes, using
-- the data declarations for Bool and Lists.

--     data Bool     =  False  |  True
--     [1]   [2]    [3] [4]   [5] [6]
--    1. Keyword "data" to signal that what follows in a data declaration,
--       or a declaration of a datatype.
--    2. Type constructor with no arguments.
--    3. Equals sign divides the type constructor from its data constructors.
--    4. A data constructor. In this case, its a data constructor that takes
--       no arguments (aka a `nullary` constructor). This is one of the
--       possible values of this type that can show up in term-level code.
--    5. The pipe denotes a sum type which indicates a logical disjunction
--       (colloqially, `or`), in what values can have that type.
--    6. Constructor for the value `True`, another nullary constructor.
--
--
--      data   [] a    =    []    |    a : [a]
--              [1]         [2]         [3]
--    1. Type constructor with an argument. An empty list has to be applied to
--       an argument in order to become a list of `something`. Here the argument
--       is a polymorphic type variable, so the list's argument can be of
--       different types
--    2. Data constructor for the empty list.
--    3. Data constructor that takes 2 arguments: an `a` and also `[a]`

-- NOTE: A type is an enumeration of constructors that have >=0 arguments...

-- However, sometimes we need the flexibility of allowing different
-- types or amounts of data to be stored in our datatypes. For those
-- times, type and data constructors may be parameterized. When a
-- constructor takes an argument, then it’s like a function in at least one
-- sense – it must be applied to become a concrete type or value. The
-- following datatypes are pseudonymous versions of real datatypes in
-- Haskell. We’ve given them pseudonyms because we want to focus
-- on the syntax, not the semantics, for now.

-- data Trivial = Trivial'
--    [1]             [2]
-- 1. Here the type constructor `Trivial` is like a constant value, but
--    at the type level. It takes no arguments and is thus nullary.
--    The Haskell Report calls these `type constants` to distinguish them from
--    type constructors that take arguments.
-- 2. The data constructor Trivial' is also like a constant value, but it
--    exists in value, term, or runtime space. These are not three different
--    things, but three different words for the same space that types serve
--    to describe.

-- data UnaryTypeCon a = UnaryValueCon a
--      [1]                     [2]
-- 1. UnaryTypeCon is a type constructor of one argument. It's a constructor
--    awaiting a type constant to be appied to, but it has no behaviour in the
--    sense that we think of functions as having. Such type-level functions exist
--    but are not covered here.
-- 2. UnaryValueCon is a data contructor of one argument awaiting a value to be
--    applied to. Again, it doesn't behave like a term-level function in the sense
--    of performing an operation on data. It's more like a box to put
--    values into. Be careful with the box/container analogy as its misleading - not
--    NOT all `type arguments` to constructors have value-level witnesses! Some
--    are phantom - covered later.

-- 11.5 Data constructors and values


data PugType = PugData
data HuskyType a = HuskyData -- `phantom datatype <- ignores `data`-constructor argument
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData      -- phantom type argument to the dataconstructor

myOtherOtherHusky :: HuskyType [[[[[Int]]]]]
myOtherOtherHusky = HuskyData -- phantom type argument to the dataconstructor

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- badDode :: DogueDeBordeaux String
-- badDoge = DogueDeBordeaux 10 --- Won't work

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- Types are known before runtime, whether through explicit declaration or
-- type inference, and that’s what makes them static types. Information about
-- types does not persist through to runtime. Data are what we’re
-- working with at runtime.
-- Types circumscribe values and in that way, they describe which values are
-- flowing through what parts of your program.


-- type constructors -- compile-time

-- -------------------- phase separation

-- data constructors -- runtime

-- Both data constructors and type constructors begin with capital
-- letters, but a constructor before the = in a datatype definition is a type
-- constructor, while constructors after the = are data constructors. Data
-- constructors are usually generated by the declaration. One tricky
-- bit here is that when data constructors take arguments, those argu-
-- ments refer to other types. Because of this, not everything referred
-- to in a datatype declaration is necessarily generated by that datatype
-- itself.

data Price          = Price Integer deriving (Show, Eq)
data Manufacturer   = Mini | Mazda | Tata deriving (Show, Eq)
data Airline        = JetAir | AirAsia | Indigo deriving (Show, Eq)
data Vehicle        = Car Manufacturer Price | Plane Airline deriving (Show, Eq)

myCar     = Car Mini (Price 1000)
urCar     = Car Mazda (Price 2000)
clownCar  = Car Tata (Price 5)
doge      = Plane Indigo

isCar :: Vehicle -> Bool
isCar (Car _ _)     = True
isCar _             = False

isPlane :: Vehicle -> Bool
isPlane (Plane _)   = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars v = map isCar v

getManufacturer :: Vehicle -> Manufacturer
getManufacturer (Car m p) = m

-- 11.7 Data constructor arities

-- Arity refers to the number of arguments a function or constructor takes.
-- Data constructors that take one argument are called unary.
-- Data constructors that take more than one argument are called `products`.

-- 11.8 What makes these datatypes algebraic?

-- The cardinality of a datatype is the number of possible values
-- it defines. That number can be as small as 0 or as large as infinite
-- (for example, numeric datatypes, lists). Knowing how many possible
-- values inhabit a type can help you reason about your programs

-- A unary data constructor takes
-- one argument. In the declaration of the datatype, that parameter
-- will be a type, not a value. Now, instead of your data constructor
-- being a constant, or a known value, the value will be constructed at
-- runtime from the argument we applied it to.
-- Datatypes that only contain a unary constructor always have the
-- same cardinality as the type they contain. In the following, Goats has
-- the same number of inhabitants as Int:
-- data Goats = Goats Int deriving (Eq, Show)
-- Anything that is a valid Int, must also be a valid argument to the
-- Goats constructor. Anything that isn’t a valid Int also isn’t a valid
-- count of Goats.
-- For cardinality this means unary constructors are the identity
-- function.

-- 11.9 newtype

-- We will now look at a way to define a type that can only ever have a
-- single unary data constructor. We use the newtype keyword to mark
-- these types, as they are different from type declarations marked with
-- the data keyword as well as from type synonym definitions marked
-- by the type keyword. Like other datatypes that have a single unary
-- constructor, the cardinality of a newtype is the same as that of the type
-- it contains.
-- A newtype cannot be a product type, sum type, or contain nullary
-- constructors, but it has a few advantages over a vanilla data dec-
-- laration. One is that it has no runtime overhead, as it reuses the
-- representation of the type it contains. It can do this because it’s not
-- allowed to be a record (product type) or tagged union (sum type).
-- The difference between newtype and the type it contains is gone by
-- the time the compiler generates the code.

-- https://stackoverflow.com/questions/991467/haskell-type-vs-newtype-with-respect-to-type-safety
-- https://www.reddit.com/r/haskell/comments/6xri4d/whats_the_difference_between_newtype_type_and_data/

data GoatsData = GoatsD Int deriving (Show, Eq)
newtype Goats = Goats Int deriving (Show, Eq)

tooManyGoats :: Goats -> Bool -- will NOT work if it were :: GoatsD -> Bool
tooManyGoats (Goats n) = n > 42

-- One key contrast between a newtype and a type alias is
-- that you can define typeclass instances for newtypes that differ from
-- the instances for their underlying type. You can’t do that for type
-- synonyms.

-- TODO: Revisit previous lesson on pg 200 - typeclasses, instances, and class def

-- Remember that a typeclass is a set of operations that can be performed on a
-- particular type, or set of types if the typeclass is polymorphic.
-- In the case of polymorphic typeclasses, instances will have to be defined
-- separately for each type that the typeclass serves.

--

class TooMany a where           -- A polymorphic typeclass
  tooMany :: a -> Bool          -- Members of this typeclass can access this fn

instance TooMany Int where      -- Definition of a member of this TC, with type Int
  tooMany n = n > 42            -- Defines the tooMany fn for an arg of type Int

newtype Goats' = Goats' Int deriving Show

instance TooMany Goats' where   -- Definition of another member of type Goats'
  tooMany (Goats' n) = n > 43   -- Defines the tooMany fn for vals of type Goats'

newtype Goats'' = Goats'' Int deriving (Show, Eq, TooMany)

-- NOTE: We we don't have to define an instance of TooMany for Goats'' that's
-- merely identical to the Int instance. We can reuse the instance that we
-- already have. This is also nice for times when we want every typeclass
-- instance to be the same EXCEPT for the one we want to change.

-- Exercises: Pg 404 Logic Goats

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass
-- for the type (Int, String). This will require adding a language
-- pragma named FlexibleInstances 4 if you do not use a newtype
-- — GHC will tell you what to do.

instance TooMany (Int, String) where
  tooMany (n, s) = (length s) > n

newtype Goats1 = Goats1 (Int, String) deriving (Show, Eq, TooMany)

-- 2. Make another TooMany instance for (Int, Int). Sum the values
-- together under the assumption this is a count of goats from two
-- fields.

-- instance TooMany (Int, Int) where
--   tooMany (n1, n2) = n1 > n2

newtype Goats2 = Goats2 (Int, Int) deriving (Show, Eq, TooMany)

-- 3. Make another TooMany instance, this time for (Num a, TooMany a)
-- => (a, a). This can mean whatever you want, such as summing
-- the two numbers together.


-- TODO: Revise instances...
-- pg404 Q3
instance (Num a, Eq a, TooMany a) => TooMany (a, a) where
  tooMany (e1, e2) = e1 == e2

newtype Goats3 a = Int a deriving (Show, Eq, TooMany)
-- TODO: check if above works


-- 11.10 Sum types

-- A product type’s cardinality is the product of the cardinalities of its inhabitants.
-- Arithmetically, products are the result of multiplication. Where a sum type
-- was expressing or, a product type expresses and.
-- For those that have programmed in C-like languages before, a
-- product is like a struct. For those that haven’t, a product is a way to
-- carry multiple values around in a single data constructor. Any data
-- constructor with two or more type arguments is a product.

-- We said previously that tuples are anonymous products. The
-- declaration of the tuple type looks like this:
-- ( , ) :: a -> b -> (a, b)
-- This is a product, like a product type: it gives you a way to en-
-- capsulate two pieces of data, of possibly (though not necessarily)
-- different types, in a single value.
-- We’ll look next at a somewhat silly sum type:

data QuantumBool =  QuantumTrue
                  | QuantumFalse
                  | QuatumBoth      deriving (Eq, Show)

data TwoQs       =  MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

-- The datatype TwoQs has one data constructor, MkTwoQs, that takes
-- two arguments, making it a product of the two types that inhabit it.
-- Each argument is of type QuantumBool, which has a cardinality of 3.

-- We could have also written the TwoQs type using a type alias and
-- the tuple data constructor. Type aliases create type constructors, not
-- data constructors:

type TwoQs' = (QuantumBool, QuantumBool)

-- Record syntax

data PersonR = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: PersonR -> String
namae (MkPerson s _) = s

-- Now let’s see how we could define a similar product type but with
-- record syntax:

data PersonR' = PersonR' {  name :: String
                          , age  :: Int }
                          deriving (Show, Eq)

data Fiction = FictionL deriving Show
data Nonfiction = NonfictionL deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show
-- Above, it is the type constructors that are the arguments to FictionBook and
-- NonfictionBook. Type values are named differently to enable noticing the
-- difference

-- Above is the sum type. Next we're going to define a type synonym called
-- AuthorName and a product type called Author.

type AuthorName = String

data Author = Author (AuthorName, BookType)

-- This isn't a sum of products, so it isn't in normal form. We can apply the
-- distributive property and rewrite Author in normal form.

-- type AuthorNameNormal = String

data AuthorNormal =   Fiction AuthorName
                    | NonFiction AuthorName
                    deriving (Show, Eq)

-- Products distribute over sums. Just as we would do with the ex-
-- pression a * (b + c), where the inhabitants of the sum type BookType
-- are the b and c, we broke those values out and made a sum of prod-
-- ucts. Now it’s in normal form because no further evaluation can be
-- done of these constructors until some operation or computation is
-- done using these types.

-- data ExprSum = Number Int
--              | Add Expr Expr -- <== ?
--              | Minus Expr
--              | Mult Expr Expr
--              | Divide Expr Expr
-- This is in normal form because it’s a sum (type) of products: (Num-
-- ber Int) + Add (Expr Expr) + ...

-- A stricter interpretation of normal form or “sum of products”
-- would require representing products with tuples and sums with
-- Either. The previous datatype in that form would look like the fol-
-- lowing:

-- TODO: The following errors out with "Cycle in type synonym declarations"

-- type Number = Int
-- type Add    = (Expr, Expr)
-- type Minus  = Expr
-- type Mult   = (Expr, Expr)
-- type Divide = (Expr, Expr)

-- type Expr   =
--               Either Number
--                 (Either Add
--                   (Either Minus
--                     (Either Mult Divide)))

data FlowerType =   Gardenia
                  | Daisy
                  | Rose
                  | Lilac
                  deriving Show

type Gardener = String

data Garden   =
      Garden Gardener FlowerType
      deriving Show

-- What is the normal form of Garden?

data GardenNormal = Gardener FlowerType deriving Show
-- TODO: Re-reminder: types, typeclasses and instances need revision


-- 11.13 Constructing and deconstructing values

-- There are essentially two things we can do with a value. We can
-- generate or construct it or we can match on it and consume it. In this
-- section will elaborate on that and how to construct values of different types.


-- We’ll start by defining a collection of datatypes:

data GuessWhat          = Chickenbutt deriving (Eq, Show)
data Id a               = MkId a deriving (Eq, Show)
data Product a b        = Product a b deriving (Eq, Show)
data Sum a b            = First a | Second b deriving (Eq, Show)
data RecordProduct a b  = RecordProduct { pfirst  :: a
                                        , psecond :: b }
                                        deriving (Eq, Show)
-- NOTE+COMMIT: The above expression is known as "Record Syntax"

-- Now that we have different sorts of datatypes to work with, we’ll
-- move on to constructing values of those types.

newtype NumCow          = NumCow Int deriving (Eq, Show)
newtype NumPig          = NumPid Int deriving (Eq, Show)

data Farmhouse          = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse'         = Product NumCow NumPig -- prodcut with 2 args

-- NOTE: Farmhouse and Farmhouse' are the same !!!

newtype NumSheep        = NumSheep Int deriving (Eq, Show)

data BigFarmhouse       = BigFarmhouse NumCow NumPig NumSheep -- product c 3 args
                          deriving (Eq, Show)

type NameS              = String
type AgeS               = Int
type LovesMud           = Bool

type PoundsOfWool       = Int

data CowInfo            = CowInfo NameS AgeS deriving (Eq, Show)
data PigInfo            = PigInfo NameS AgeS LovesMud deriving (Eq, Show)
data SheepInfo          = SheepInfo NameS AgeS PoundsOfWool
                          deriving (Eq, Show)

data Animal             =
                          Cow CowInfo
                        | Pig PigInfo
                        | Sheep SheepInfo
                        deriving (Eq, Show)
-- Alternately...
data Animal'            = Sum CowInfo (Sum PigInfo SheepInfo)

-- Constructing Values - 418

-- We just define
-- trivialValue to be the nullary data constructor Chickenbutt and we
-- have a value of the type GuessWhat.

trivialValue :: GuessWhat
trivialValue = Chickenbutt

data IdPP a = MkIdPP a deriving (Eq, Show)

idInt :: IdPP Integer
idInt = MkIdPP 10

idIdentity :: IdPP (a -> a)
idIdentity = MkIdPP $ \x -> x

type Awesome    = Bool
type Name       = String

person :: Product Name Awesome
person = Product "Dlido" True

-- constructors with data synonyms
data Twitter = Twitter deriving (Eq, Show)

data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data SocialNetwork' = TwitterP | AskFmP deriving (Eq, Show)

-- constructors with type synonyms

type TwitterT = String
type AskFmT = String

twitter :: Sum TwitterT AskFmT
twitter = First "TwitterT"

askFm :: Sum TwitterT AskFmT
askFm = First "AskFmT"

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.0001

-- Remember the data consttructor for this is:
-- data RecordProduct a b  = RecordProduct { pfirst  :: a
--                                         , psecond :: b }
--                                         deriving (Eq, Show)


-- NOTE: Printing myRecord gives:
--    RecordProduct {pfirst = 42, psecond = 1.0e-4}
-- For this reason you can rewrite myRecord as:

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42,
                            psecond = 0.001 }

data OperatingSystem =    GnuPlusLinux 
                        | OpenBSD
                        | Mac
                        | Windows
                        deriving (Eq, Show)
        
data ProgrammingLanguage =  Haskell
                          | Agda
                          | Idris
                          | PureScript
                          deriving (Eq, Show)

data Programmer     =  Programmer { os :: OperatingSystem
                                  , lang :: ProgrammingLanguage }
                                  deriving (Eq, Show)
                                
-- Excercise: Programmers - p423
-- Write a function that generates all possible values of Programmer. Use
-- the provided lists of inhabitants of OperatingSystem and ProgrammingLanguage.

allOSs :: [OperatingSystem]
allOSs = [ GnuPlusLinux
         , OpenBSD
         , Mac
         , Windows
        ]
allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- answer:
allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOSs, y <- allLanguages]
---

data ThereYet = There Integer Float String Bool deriving (Eq, Show)

-- building the types to plug in to "ThereYet"

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "Woohoooo"

yuss :: ThereYet
yuss = notQuite False

-- Notice the way our types progressed.

-- There ::   Integer -> Float -> String -> Bool -> ThereYet
-- nope ::               Float -> String -> Bool -> ThereYet
-- notYet ::                      String -> Bool -> ThereYet
-- notQuite ::                              Bool -> ThereYet
-- yuss ::                                          ThereYet

-- Percolate values through your programs, not bottoms.



-- Deconstructing values - p425

-- When we discussed folds, we mentioned the idea of catamorphism.
-- We explained that catamorphism was about deconstructing lists. This
-- idea is generally applicable to any datatype that has values.

newtype FarmerName  = FarmerName String deriving Show
newtype Acres       = Acres Int deriving Show

data FarmerType     = DairyFarmer
                    | WheatFarmer
                    | SoyFarmer deriving Show

data Farmer = Farmer FarmerName Acres FarmerType deriving Show

-- Now we write a check that deconstructs using pattern matching
isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer)  = True
isDairyFarmer _                         = False

-- Now we construct a product that uses the record syntax
-- NOTE: observe the following carefully
data FarmerRec = FarmerRec { farmername   :: FarmerName
                           , acres        :: Acres
                           , farmerType   :: FarmerType } deriving Show
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmerRec = case farmerType farmerRec of -- This.
  DairyFarmer -> True
  _           -> False

-- Accidental bottoms from records

-- You can easily propagate bottoms through record types.
-- Please do not do this:

data AutomobileWrong    = Null' -- Added a "'" to separate it from the correct version 
                        | CarWrong { make' :: String
                              , model' :: String
                              , year' :: Integer }
                              deriving (Eq, Show)
-- This is a terrible thing to do, for a couple of reasons. One is this
-- Null nonsense. Haskell offers you the perfectly lovely datatype Maybe,
-- which you should use instead. Secondly, consider the case where
-- one has a Null value, but you’ve used one of the record accessors:
-- Prelude> make Null
-- "*** Exception: No match in record selector make

data CarCorrect = CarCorr { make :: String 
                      , model :: String
                      , year :: Integer }
                      deriving (Eq, Show)

-- The Null is still not great, but
-- we're leaving it in to make a point
data AutomobileCorr = Null | Automob CarCorrect

-- p249 Function type is exponential
-- Given a function a -> b, we can calculate the
-- inhabitants with the formula nb ^ na, where:
-- nb -> number of possible values in b, and
-- na -> number of possible values of a

data Quantum =  Yes
              | No
              | Both deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantumSum2 :: Either Quantum Quantum
quantumSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- Consider the following function:
convert1 :: Quantum -> Bool
-- convert = undefined

-- According to the equality of a -> b = b^a there should be 2^3 or 8
-- implementations of this function. For a -> b -> c it's (c ^ b) ^ a. 
-- Does this hold? Write it out and prove it for yourself.

-- skipping type signature here
convert1 q = case q of
              Yes   -> True
              No    -> False
              Both  -> False

-- skipping type signature here
convert2 q = case q of
              Yes   -> True
              No    -> False
              Both  -> True

-- similarly for convert3, and so on, we can see that the number of such functions is 8

-- p433: Higher-kinded datatypes

-- Kinds are the types of type constructors, primarily encoding the number of
-- arguments they take. The default kind in Haskell is *. Kind signatures
-- work like type signatures, using the same :: and -> syntax, but there
-- are only a few kinds and you’ll most often see *.
--
-- Kinds are not types until they are fully applied. Only types have
-- inhabitants at the term level. The kind * -> * is waiting for a single *
-- before it is fully applied. The kind * -> * -> * must be applied twice
-- before it will be a real type. This is known as a higher-kinded type.
-- Lists, for example, are higher-kinded datatypes in Haskell.
--
-- Because types can be generically polymorphic by taking type
-- arguments, they can be applied at the type level.


-- Silly polymorphic product type
-- this is identical to (a, b, c, d)
data Silly a b c d = MkSilly a b c d deriving Show

-- in ghci:
-- Prelude> :kind Silly
-- Silly :: * -> * -> * -> * -> *
-- Prelude> :kind Silly Int
-- Silly Int :: * -> * -> * -> *
-- Prelude> :kind Silly Int String
-- Silly Int String :: * -> * -> *
-- Prelude> :kind Silly Int String Bool
-- Silly Int String Bool :: * -> *
-- Prelude> :kind Silly Int String Bool String
-- Silly Int String Bool String :: *

-- p435-11.6 Lists are polymorphic
-- 
-- The Haskell definition of lists is as follows.
--
-- data [] a = [] | a : [a]
--
-- 1. '[]' is the special syntax for the Type Constructor of a list.
-- 2. 'a' is the single type argument to []. This shows the type to be 
--    polymorphic
-- 3. The definition can be a single empty list constructor (Nil,) or...
-- 4. A single value of type a CONSed with [] a / [a] (which is
--    the representaion for the rest of the list)

-- Infix type and Data constructors

-- Any operator that starts with a colon (:) must be an `infix type` OR `data
-- constructor`. 
-- All `infix data constructors` must start with a -- colon. 
-- The type constructor of functions, (->), is the only infix type
-- constructor that doesn’t start with a colon. Another exception is that
-- they cannot be :: as this syntax is reserved for type assertions.
--
-- You can add non-alphanumeric characters to get your own unique 
-- infix type or data constructor.
-- Here’s an example of an infix data constructor:

data ExampleInfixDataConstructor a b = a :%-%: b deriving (Eq, Show)

-- Example o/p:
-- Prelude> 1 :%-%: 2
-- 1 :%-%: 2
-- Prelude> k = 1 :%-%: 2
-- Prelude> :t k
-- k :: (Num a, Num b) => ExampleInfixDataConstructor a b
--

-- A value of type ExampleInfixDataConstructor would be a product of 
-- two arguments, one of type a and one of type b.
-- Whether or not you choose to use infix data constructors, type
-- constructors, or typeclass names is down to aesthetic preference.

-- The following example defines the list type without using an infix
-- operator

data ListNonInfix a = Nil | Cons a (ListNonInfix a) deriving Show

-- Example usage:

-- Prelude> y = Cons 123 Nil
-- Prelude> y
-- Cons 123 Nil
-- Prelude> yy = Cons 234 y
-- Prelude> yy
-- Cons 234 (Cons 123 Nil)
-- Prelude> yyy = Cons 456 yy
-- Prelude> yyy
-- Cons 456 (Cons 234 (Cons 123 Nil))

-- checking out the kinding of our list type, and comparing it with []:

-- Prelude> :kind ListNonInfix
-- ListNonInfix :: * -> *

-- Prelude> :kind []
-- [] :: * -> *

-- Prelude> :kind ListNonInfix
-- ListNonInfix Int :: *

-- Prelude>:kind [Int]
-- *

-- 11.7 Binary Tree
-- Now we turn our attention to a type similar to list. The type con-
-- structor for binary trees can take an argument, and it is also recursive
-- like lists:

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- Example usage:

-- *Main> let ll = Leaf
-- *Main> :t ll
-- ll :: BinaryTree a
-- *Main> let lll = Node Leaf 10 Leaf
-- *Main> :t lll
-- lll :: Num a => BinaryTree a
-- *Main> let lll = Node Leaf 10 (Node Leaf 12 Leaf)
-- *Main> :t lll
-- lll :: Num a => BinaryTree a
-- *Main> lll
-- Node Leaf 10 (Node Leaf 12 Leaf)


-- insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
-- insert' b leaf = Node Leaf b Leaf
-- insert' b (Node left a right)
--   | b == a  = Node left a right
--   | b < a   = Node (insert' b left) a right
--   | b > a   = Node left a (insert' b right)

insert'' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert'' b Leaf = Node Leaf b Leaf
insert'' b (Node left a right)
  | b == a        = Node left a right
  | b > a         = Node left a (insert'' b right)
  | b < a         = Node (insert'' b left) a right

-- Example usage: 

-- *Main> let i1 = insert'' 0 Leaf
-- *Main> i1
-- Node Leaf 0 Leaf
-- *Main> let l2 = insert'' 3 i1
-- *Main> l2
-- Node Leaf 0 (Node Leaf 3 Leaf)
-- *Main> let l3 = insert'' 1 l2
-- *Main> l3
-- Node Leaf 0 (Node (Node Leaf 1 Leaf) 3 Leaf)

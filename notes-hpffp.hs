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

-- -- custom ordering for DayOfWeek s.t. Friday is max and all else eq



data Date = Date DayOfWeek Int deriving Show

{- instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sun Sun = True
  (==) _ _     = False -}

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

check' :: Ord a => a -> a -> Bool -- Eq typeclass is suffic Ord is overkill for Eq comparsn
check' a a' = a == a'

-- -- self problem: create stringSucc which
-- -- increments the chars of a string from LSB to MSB
{- 
stringSucc :: String -> String
stringSucc s =  -}

-- example of bad things... ** TODO -> wut?

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

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

  -- example of worse things... ** TODO: wut? wrt default vals in typeclass defs

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
-- Foldl (LEFT)

myFoldl :: (b -> a -> b) -> b -> [] a -> b
myfoldl f acc []      = acc       -- <== This.
myFoldl f acc (a:as)  = myFoldl f (f acc a) as

-- contrast with foldr (RIGHT)

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
  , DbString "hello db!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

isDbDate :: DatabaseItems -> Bool
isDbDate a = undefined

-- Q1. Write a function that filters for DbDate values and returns a list
-- of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbItem = undefined-- filter ()

-- Q2. Write a function that filters for DbNumber values and returns a list
-- of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = undefined

-- Q3. 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined

-- Q4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = undefined

-- Q5. Write a function that gets the average of the DbNumber values.
-- You'll probably need to use fromIntegral
-- to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb = undefined


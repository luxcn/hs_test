module Lib.Lib where

someFunc :: IO ()
someFunc = putStrLn "hello"

sumList (x:xs) = foldl (+) 0 xs

data Customer = Customer
  { customerID :: Int
  , customerName :: String
  , customerAddress :: String
  } deriving (Show)

square :: [Double] -> [Double]
square (x:xs) = x * x : square xs
square [] = []

niceSum :: [Integer] -> Integer
niceSum = foldl myPlus 0

myPlus :: Integer -> Integer -> Integer
myPlus a b = a + b

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl step zero (x:xs) = myFoldl step (step zero x) xs
myFoldl _ zero [] = zero

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ zero [] = zero
myFoldr step zero (x:xs) = step x (myFoldr step zero xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = filter f xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

--maximum' :: (Ord a) => [a] -> a
--maximum' [] = error "It's a empty list!"
--maximum' [x] = x
--maximum' (x:xs)
--  | x > maxTail = x
--  | otherwise = maxTail
--  where
--    maxTail = maximum' xs
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyMulti :: (Num n, Ord n) => (a -> a) -> a -> a -> n -> a
applyMulti f x zero n
  | n <= 0 = zero
  | otherwise = f (applyMulti f x zero (n - 1))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filterStatus
  | otherwise = filterStatus
  where
    filterStatus = filter' f xs

data Point =
  Point Float
        Float
  deriving (Show)

data Shape
  = Circle Point
           Float
  | Rectangle Point
              Point

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Show,Eq)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)



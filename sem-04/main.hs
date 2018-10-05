module Main where

import Debug.Trace

-- | Задание 1.
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | Задание 2.
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- | Задание 3.
collatz :: Integer -> Integer
collatz n 
  | n == 1          = 1
  | n `mod` 2 == 0  = collatz (n `div` 2)
  | otherwise       = collatz (3 * (n + 1))

-- | Задание 4.
doubleFac :: Integer -> Integer
doubleFac n 
  | n < 2       = 1
  | otherwise   = n * doubleFac (n - 2) 

-- | Задание 5.
fib :: Integer -> Integer 
fib = go 1 1 
    where 
        go a _ 0 = a
        go a b n = go b (a + b) (n - 1)

-- | Задание 6.
numberAndSumsOfDigits :: Integer -> (Integer, Integer)
numberAndSumsOfDigits 0 = (1, 0)
numberAndSumsOfDigits n = go (0, 0) n
    where
        go result 0 = result
        go (c, s) n = go (c + 1, s + n `mod` 10) (n `div` 10)

-- | Задание 7.
myhof :: (a -> b) -> (a -> c) -> a -> (b, c)
myhof f g x = (f x, g x)

-- | Задание 8.
integral :: (Double -> Double) -> Double -> Double -> Double 
integral f a b = sum areas
    where
        n     = 1000
        dx    = (b - a) / n
        xs    = [a, a+dx..b]
        areas = zipWith (\l r -> dx * (f l + f r) / 2) xs (tail xs)

check :: Bool -> IO ()
check True  = pure ()
check False = error "test failed"

main = do
    check $  1 == fac 0
    check $  1 == fac 1
    check $  2 == fac 2
    check $  6 == fac 3
    check $ 24 == fac 4
    check $  1 == doubleFac 0
    check $  1 == doubleFac 1
    check $  2 == doubleFac 2
    check $  3 == doubleFac 3
    check $  8 == doubleFac 4
    check $ 15 == doubleFac 5
    check $ 48 == doubleFac 6
    check $  1 == fib 0
    check $  1 == fib 1
    check $  2 == fib 2
    check $  3 == fib 3
    check $  5 == fib 4
    check $  8 == fib 5
    check $ (1, 0) == numberAndSumsOfDigits 0
    check $ (1, 1) == numberAndSumsOfDigits 1
    check $ (2, 2) == numberAndSumsOfDigits 11
    check $ (3, 1) == numberAndSumsOfDigits 100
    check $ (< 0.00001) $ abs $       1 - integral (const 1) 0 1
    check $ (< 0.00001) $ abs $     0.5 - integral id 0 1
    check $ (< 0.00001) $ abs $ (1 / 3) - integral (\x -> x * x) 0 1
    check $ (< 0.00001) $ abs $    (-2) - integral sin pi 0

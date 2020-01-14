module Ex7 where

import Data.List (intersperse)


tensDigit :: Integral a => a -> a -> a 
-- tensDigit x = d
--     where xLast = x `div` 10
--           d = xLast `mod` 10
tensDigit x position =  
    ost x `div` position
    where ost a = snd $ a `divMod` (position * 10)


dodgy x y = x + y * 10 
oneIsOne = dodgy 1 
oneIsTwo = (flip dodgy) 2


foldBool :: a -> a -> Bool -> a 
foldBool x y b = case b of 
    True -> x
    False -> y
foldBool' :: a -> a -> Bool -> a 
foldBool' x y b 
    | b == True = x
    | otherwise = y

-- applyTimes 5 (+1) 5
-- (+1) (applyTimes 4 (+1) 5)
-- (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))

recSum :: (Eq a, Num a) => a -> a
recSum 1 = 1
recSum 2 = 3
recSum x = recSum(x - 1) + x

recMult :: (Integral a) => a -> a -> a
recMult first second = go first second 0
    where go f s r
            | s == 0 = r
            | otherwise = go f (s - 1) (f + r)



data DividedResult = Result Integer | DividedByZero 
    deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = 
    Result (negateIf go num denom 0)
    where 
        go n d count
            | n < d = count
            | otherwise =
                go (n - d) d (count + 1)
        negateIf f a b c
            | (a > 0 && b > 0) = f a b c
            | (a < 0 && b < 0) = f (negate a) (negate b) c
            | (a > 0 && b < 0) = negate (f a (negate b) c)
            | (a < 0 && b > 0) = negate (f (negate a) b c) 
            | otherwise = 123

mc91 :: Integral a => a -> a
mc91 x 
        | x > 100 = x - 10
        | otherwise = mc91(mc91(x + 11)) 


digitToWord :: Int -> String 
digitToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> "zero"
    
digits :: Int -> [Int] 
digits n = go n [] where 
    go n' l = case n' == 0 of
        True -> l
        False -> go (div n' 10) (mod n' 10 : l)

wordNumber :: Int -> String 
wordNumber n = cr (intersperse "-" (map digitToWord $ digits n)) "" 
    where cr arr r = 
            case length arr == 0 of
            True -> r
            False -> cr (drop 1 arr) (r ++ head arr)
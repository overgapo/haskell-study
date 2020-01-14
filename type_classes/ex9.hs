module Ex9 where

import Data.Char

myWords :: String -> [String]
myWords stringWithSpaces = go stringWithSpaces []
    where 
        go s r = 
            case s == "" of
                True -> r
                False -> go (drop 1 $ dropWhile (/=' ') s) ((takeWhile (/=' ') s) : r) 

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- a = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- bottoms exercies WTF
-- 9.9.6 - WTF

zip' :: [a] -> [b] -> [(a, b)] 
zip' [] _  = [] 
zip' _ []  = [] 
zip' (a:as) (b:bs) = (a, b) : zip' as bs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = (f a b) : zipWith' f as bs

zip'' :: [a] -> [b] -> [(a, b)]
zip'' a b = zipWith' (\x -> \y -> (x, y)) a b

filterUpper :: String -> String
filterUpper [] = []
filterUpper (x:xs)
    | isUpper x = x: filterUpper xs
    | otherwise = filterUpper xs

firstUpper :: String -> String
firstUpper [] = []
firstUpper (x:xs) = toUpper x : xs

allUpper :: String -> String
allUpper [] = []
allUpper (x:xs) = toUpper x : allUpper xs


module TypeCheck where

import Data.List

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)



chk :: Eq b => (a -> b) -> a -> b -> Bool 
chk aToB a b = aToB a == b 

arith :: Num b
    => (a -> b) 
    -> Integer 
    -> a
    -> b
arith aToB i a = 
    (aToB a) - (fromInteger i)
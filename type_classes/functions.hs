module Functions where

addOneIfOdd n = case odd n of 
    True -> f n 
    False -> n
    where f =  \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x


k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2)) 
k3 = k (3, True)


f :: (a, b, c)
    -> (d, e, f)
    -> ((a, d), (c, f))
f (x, _, y) (z, _, p) = ((x, z), (y, p)) 

functionC x y = 
    case x > y of 
        True -> x 
        False -> y

ifEvenAdd2 n = 
    case even n of
        True -> n+2 
        False -> n

nums x =
    case compare x 0 of
    LT -> -1 
    GT -> 1
    _ -> 0
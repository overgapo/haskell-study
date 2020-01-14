module Practice where

mult = x * y
    where 
        x = 4
        y = 5

f1 = x * 3 + y
    where   
        x = 3
        y = 1000

f2 = z / x + y
    where
        x = 7
        y = negate x
        z = y * 10
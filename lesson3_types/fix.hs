module Sing where

fstString :: [Char] -> [Char]
    
fstString x = x ++ " in the rain"
sndString :: String -> String
sndString x = x ++ " over the rainbow"
sing = if (x < y) then fstString x else sndString y
    where 
        x = "Singin"
        y = "Somewhere"
module Arith3Broken where
    
main :: IO () 
main = do
    putStrLn ("1" ++ "2")
    putStrLn "10"
    print (negate (-1) :: Int)
    print ((+) 0 blah)
        where blah = negate 1
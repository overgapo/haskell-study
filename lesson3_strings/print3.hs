module Print3 where 
    
myGreeting :: String
myGreeting = "hello" ++ " world!" 

hello :: String
hello = "hello"

world :: [Char] 
world = "world!"

main :: IO ()

main = do
    putStrLn myGreeting
    putStrLn mySecondGreeting
    where mySecondGreeting = concat [hello, " ", world]
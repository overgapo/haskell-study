{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ex11 where

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)
data Size = Small | Medium | Big 
    deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
    deriving (Eq, Show)
data Vehicle = 
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000) 
urCar = Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000) 
doge = Plane PapuAir Small

isCar :: Vehicle -> Bool 
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool 
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool] 
areCars [] = []
areCars xs = map isCar xs

getManu :: Vehicle -> Manufacturer 
getManu (Car Mini _) = Mini
getManu (Car Mazda _) = Mazda
getManu (Car Tata _) = Tata

--


class TooMany a where tooMany :: a -> Bool
instance TooMany Int where tooMany n = n > 42
instance TooMany (Int, String) 
    where tooMany (n, s) = n > 10 && length s > 10
instance TooMany (Int, Int) 
    where tooMany (a, b) = a + b > 10
-- instance TooMany (Num a, TooMany a) => (a, a)
    -- where tooMany (a,  b) = a + b > 10

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
newtype NumberString = NumberString (Int, String) deriving (Eq, Show, TooMany)
newtype IntInt = IntInt (Int, Int) deriving (Eq, Show, TooMany)